$PBExportHeader$w_maed_spro_ordenventacomenca.srw
forward
global type w_maed_spro_ordenventacomenca from w_mant_encab_deta_csd
end type
type cb_retiro from commandbutton within w_maed_spro_ordenventacomenca
end type
end forward

global type w_maed_spro_ordenventacomenca from w_mant_encab_deta_csd
string title = "Orden de Venta"
string menuname = ""
cb_retiro cb_retiro
end type
global w_maed_spro_ordenventacomenca w_maed_spro_ordenventacomenca

type variables
w_mant_deta_spro_ordenventacomdeta w_mantencion

uo_plantadesp		iuo_planta

datawindowchild	idwc_planta, idwc_especie, idwc_categoria, idwc_frio, &
                  idwc_vende, idwc_venderet

Boolean				ib_Modifica, ib_AutoCommit
String 				is_rut

DataStore			ids_retiroenca, ids_retirodeta
end variables

forward prototypes
public function boolean existemovimiento (long al_numero)
public subroutine activadatosretiro (long al_numero)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine prodrechazado (string as_rut)
public function boolean existevendedor (integer ai_vende)
public subroutine habilitaencab (boolean habilita)
public subroutine buscacliente ()
public function boolean existecliente (string as_rut)
public subroutine habilitaingreso ()
end prototypes

public function boolean existemovimiento (long al_numero);Integer	li_tipodoc, li_planta
Long     ll_nrodocto

li_planta  = dw_2.Object.plde_codigo[1]
li_tipodoc = integer(istr_mant.argumento[2])

SELECT	odfc_numero
	INTO	:ll_nrodocto
	FROM	dba.spro_ordenventacomenca
	WHERE	plde_codigo	=	:li_planta
	AND	odfc_numero	=	:al_numero;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_ordenventacomenca")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN False
END IF

RETURN True
end function

public subroutine activadatosretiro (long al_numero);Integer li_planta, li_TipoRetiro, respuesta
Long    ll_NumeroRetiro

li_planta = Integer(istr_mant.argumento[1])
li_TipoRetiro = Integer(istr_mant.argumento[2])

SELECT Distinct oret_numero INTO :ll_NumeroRetiro
  FROM dba.spro_ordenretiroventaenc
 WHERE plde_codigo = :li_planta
   AND tdop_codigo = :li_TipoRetiro
	AND odfc_numero = :al_Numero;
	
IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Orden de Retiro Encabezado")

ELSEIF SqlCa.SQLCode <> 100 THEN

	IF ll_NumeroRetiro<>0  THEN
	 istr_mant.argumento[6]	= String(ll_NumeroRetiro)
	 DO
		IF ids_retiroenca.Retrieve(li_Planta,li_TipoRetiro, ll_NumeroRetiro) = -1 OR &
		   ids_retirodeta.Retrieve(li_planta,li_TipoRetiro, ll_NumeroRetiro) = -1 THEN
		
		  	respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			
	   END IF
	  LOOP WHILE respuesta = 1
	  IF respuesta = 2 THEN Close(w_maed_spro_ordenventacomenca)	

	END IF	
END IF

end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
Integer li_error=1

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			 IF ids_retirodeta.RowCount() >0 THEN
			   IF ids_retirodeta.Update(True, False) = 1 THEN  li_error=1
				IF ids_retiroenca.Update(True, False) = 1 THEN  li_error=1
			 END IF 
			 IF li_Error=1 THEN
				Commit;
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				ELSE
					lb_Retorno	=	True
			
					dw_2.ResetUpdate()
					dw_1.ResetUpdate()		
					ids_retirodeta.ResetUpdate()
					ids_retiroenca.ResetUpdate()
										
				END IF	
			ELSE
				RollBack;
				F_ErrorBaseDatos(sqlca, This.Title)
		   END IF
		ELSE
			RollBack;
			F_ErrorBaseDatos(sqlca, This.Title)
   	END IF
	ELSE
		RollBack;
		F_ErrorBaseDatos(sqlca, This.Title)
   END IF
ELSE
	li_Error=1
	IF dw_2.Update(True, False) = 1 THEN		 				 //Encabezado Orden
		IF dw_1.Update(True, False) = 1 THEN					 //Detalle Orden
		   IF ids_retirodeta.RowCount() > 0 THEN
			  IF ids_retiroenca.Update(True, False) = 1 THEN	li_error=1 //Encabezado Retiro
			  IF ids_retirodeta.Update(True, False) = 1 THEN  li_error=1 //Detalle Retiro
			END IF
			IF li_Error=1 THEN
				Commit;
					
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
						
					ids_retirodeta.ResetUpdate()
					ids_retiroenca.ResetUpdate()
					dw_2.ResetUpdate()
					dw_1.ResetUpdate()
					
					cb_retiro.Visible = TRUE
					
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public subroutine prodrechazado (string as_rut);String ls_nombre, ls_Null

SetNull(Ls_Null)

SELECT	pro.prod_nombre
	INTO	:ls_nombre
	FROM	dba.clienprove as cli, dba.productores pro
	WHERE	cli.clpr_nrorut	=	:as_rut
	And   pro.prod_codigo	=	cli.prod_codigo
	And   pro.prod_rechaz	=	1;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Proveedores")

ELSEIF SqlCa.SQLCode = 0 THEN
   dw_2.SetItem(1,"prod_rechaz",1)
END IF

end subroutine

public function boolean existevendedor (integer ai_vende);String ls_nombre

SELECT	vend_nombre
	INTO	:ls_nombre
	FROM	dba.vendedores
	WHERE	vend_codigo	=	:ai_vende;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Vendedores")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	messagebox("Error de Datos","El Código Ingresado no existe para Vendedores. Ingrese Otro.")
	RETURN False
END IF

RETURN True
end function

public subroutine habilitaencab (boolean habilita);

IF Habilita THEN
	

	dw_2.Object.odfc_numero.Protect 				=	0
	dw_2.Object.odfc_numero.BackGround.Color 	=	RGB(255,255,255)
	dw_2.Object.clpr_rut.Protect	 				=	0
	dw_2.Object.clpr_rut.BackGround.Color 		=	RGB(255,255,255)
	dw_2.Object.odfc_fecham.Protect 				=	0
	dw_2.Object.odfc_fecham.BackGround.Color 	=	RGB(255,255,255)
	dw_2.Object.odfc_tipret.Protect 				=	0
	dw_2.Object.odfc_tipret.BackGround.Color 	=	RGB(255,255,255)
	dw_2.Object.odfc_valpar.Protect 				=	0
	dw_2.Object.odfc_valpar.BackGround.Color 	=	RGB(255,255,255)
	dw_2.Object.vend_codigo.Protect 				=	0
	dw_2.Object.vend_codigo.BackGround.Color 	=	RGB(255,255,255)
	dw_2.Object.odfc_pesfac.Protect 				=	0
	dw_2.Object.odfc_pesfac.BackGround.Color 	=	RGB(255,255,255)
	dw_2.Object.fpmi_codigo.Protect 				=	0
	dw_2.Object.fpmi_codigo.BackGround.Color 	=	RGB(255,255,255)
	
	dw_2.Object.b_cliente.visible					=	1
	
ELSE
	
	dw_2.Object.odfc_numero.Protect 				=	1
	dw_2.Object.odfc_numero.BackGround.Color 	=	rgb(166,180,210)
	dw_2.Object.clpr_rut.Protect	 				=	1
	dw_2.Object.clpr_rut.BackGround.Color 		=	rgb(166,180,210)
	dw_2.Object.odfc_fecham.Protect 				=	1
	dw_2.Object.odfc_fecham.BackGround.Color 	=	rgb(166,180,210)
	dw_2.Object.odfc_tipret.Protect 				=	1
	dw_2.Object.odfc_tipret.BackGround.Color 	=	rgb(166,180,210)
	dw_2.Object.odfc_valpar.Protect 				=	1
	dw_2.Object.odfc_valpar.BackGround.Color 	=	rgb(166,180,210)
	dw_2.Object.vend_codigo.Protect 				=	1
	dw_2.Object.vend_codigo.BackGround.Color 	=	rgb(166,180,210)
	dw_2.Object.odfc_pesfac.Protect 				=	1
	dw_2.Object.odfc_pesfac.BackGround.Color 	=	rgb(166,180,210)
	dw_2.Object.fpmi_codigo.Protect 				=	1
	dw_2.Object.fpmi_codigo.BackGround.Color 	=	rgb(166,180,210)
	
	dw_2.Object.b_cliente.visible					=	0
	
END IF	
end subroutine

public subroutine buscacliente ();Str_busqueda	lstr_busq

lstr_busq.Argum[1]	=	'1'

OpenWithParm(w_busc_clienprove, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[10] = "" THEN
	dw_2.SetColumn("clpr_rut")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clpr_rut[1]			=	lstr_busq.argum[10]
	dw_2.Object.clpr_nombre[1]		=	lstr_busq.argum[2]
	dw_2.SetFocus()
	prodrechazado(lstr_busq.argum[10])
END IF

RETURN
end subroutine

public function boolean existecliente (string as_rut);String ls_nombre
Integer li_tipoan

SELECT	clpr_nombre, clpr_tipoan
	INTO	:ls_nombre, :li_tipoan
	FROM	dba.clienprove
	WHERE	clpr_rut	=	:as_rut;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla ClienProve")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	messagebox("Error de Datos","El Rut Ingresado no existe para Clientes. Ingrese Otro.")
	RETURN False
END IF

IF li_tipoan = 1 THEN
	dw_2.SetItem(1,"clpr_nombre",ls_nombre)
ELSE
	MessageBox("Error de Datos","El Rut Ingresado no es el del tipo de análisis requerido. Ingrese Otro.")	
	RETURN False
END IF	

RETURN True
end function

public subroutine habilitaingreso ();Date			ld_fecha
Boolean		lb_estado = True

dw_2.AcceptText()

IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
	IsNull(dw_2.Object.clpr_rut[1])    OR dw_2.Object.clpr_rut[1] 	  = "" OR &
	IsNull(dw_2.Object.odfc_fecham[1]) OR dw_2.Object.odfc_fecham[1] = ld_fecha OR &
	IsNull(dw_2.Object.odfc_tipret[1]) OR dw_2.Object.odfc_tipret[1] = 0 OR &
	IsNull(dw_2.Object.odfc_valpar[1]) OR dw_2.Object.odfc_valpar[1] = 0 OR &
	IsNull(dw_2.Object.vend_codigo[1]) OR dw_2.Object.vend_codigo[1] = 0 OR &
	IsNull(dw_2.Object.fpmi_codigo[1]) OR dw_2.Object.fpmi_codigo[1] = 0 OR &
	IsNull(dw_2.Object.odfc_diagra[1]) OR dw_2.Object.odfc_diagra[1] < 0 THEN
	lb_estado = False

END IF

pb_ins_det.Enabled = lb_estado
end subroutine

on w_maed_spro_ordenventacomenca.create
int iCurrent
call super::create
this.cb_retiro=create cb_retiro
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_retiro
end on

on w_maed_spro_ordenventacomenca.destroy
call super::destroy
destroy(this.cb_retiro)
end on

event open;/*
   	istr_mant.Argumento[1] = Planta
		istr_mant.Argumento[2] = 7 para orden de Retiro //sin uso
		istr_mant.Argumento[3] = Numero de Orden de Venta
		istr_mant.Argumento[4] = Cliente
		istr_mant.Argumento[5] = Fecha de Orden de Venta
		istr_mant.Argumento[6] = Orden de Retiro si Orden Venta es Tipo Ret. Total
		istr_mant.Argumento[7] = Tipo de Salida
		
*/
x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

ids_retiroenca			=  Create Datastore
ids_retirodeta			=  Create Datastore
ids_retiroenca.DataObject = "dw_mant_spro_ordenretiroenca"
ids_retirodeta.DataObject = "dw_mues_spro_ordenretiroventadeta"

//planta
dw_2.Getchild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve()

//Especie
dw_1.GetChild("espe_codigo",idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve()

//Vendedores
dw_2.GetChild("vend_codigo",idwc_vende)
idwc_vende.SetTransObject(SQLCA)
IF idwc_vende.Retrieve(0) = 0 THEN
	idwc_vende.InsertRow(0)
END IF	

ids_retiroenca.GetChild("vend_codigo",idwc_venderet)
idwc_venderet.SetTransObject(SQLCA)
IF idwc_venderet.Retrieve(0) = 0 THEN
	idwc_venderet.InsertRow(0)
END IF

//Categoria
dw_1.GetChild("cate_codigo",idwc_categoria)
idwc_categoria.SetTransObject(SQLCA)
idwc_categoria.Retrieve()

//Frio
dw_1.GetChild("frio_tipofr",idwc_frio)
idwc_frio.SetTransObject(SQLCA)
idwc_frio.Retrieve()

dw_1.SetTransObject(SQLCA)
dw_2.SetTransObject(SQLCA)
ids_retiroenca.SetTransObject(SQLCA)
ids_retirodeta.SetTransObject(SQLCA)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								

buscar	= "Especie:Nespe_codigo,Variedad:Nvari_codigo"
ordenar	= "Especie:espe_codigo,Variedad:vari_codigo"
end event

event ue_nuevo();Long		ll_modif
String   ls_fecha

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_2.GetNextModified(0, Primary!)
		
			IF dw_1.RowCount() > 0 and ll_Modif > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()
dw_1.Object.ofcd_bultos.visible = TRUE
dw_1.Object.ofcd_tkilos.visible = FALSE
dw_1.Object.t_kilos.visible 	  = FALSE
			
ids_retiroenca.reset()
ids_retirodeta.reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

HabilitaEncab(True)

cb_retiro.Visible = FALSE

ls_fecha = string(Today(),'dd/mm/yyyy')

istr_mant.Argumento[1] = string(gstr_paramplanta.codigoplanta)
istr_mant.Argumento[2] = "7"
istr_mant.Argumento[3] = ""
istr_mant.Argumento[4] = ""
istr_mant.Argumento[5] = ls_fecha
istr_mant.Argumento[6] = ""
istr_mant.Argumento[7] = "1"

dw_2.SetItem(1,"plde_codigo", gstr_paramplanta.codigoplanta)
dw_2.SetItem(1,"odfc_fecham", Date(Mid(ls_Fecha,1,10)))


dw_2.SetColumn("odfc_numero")
dw_2.SetFocus()


end event

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta
String	ls_Null
SetNull(ls_Null)

DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]), &
										  Long(istr_mant.Argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		
		DO
			ll_fila_d	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
												  Long(istr_mant.Argumento[3]))
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				
				IF dw_2.RowCount() > 0 THEN
					IF dw_2.Object.odfc_pesfac[1] = 1 THEN
						dw_1.Object.ofcd_bultos.visible = TRUE
						dw_1.Object.ofcd_tkilos.visible = FALSE
						dw_1.Object.t_kilos.visible 	  = FALSE
						istr_mant.Argumento[7] = "1"
					ELSE
						dw_1.Object.ofcd_bultos.visible = FALSE
						dw_1.Object.ofcd_tkilos.visible = TRUE
						dw_1.Object.t_kilos.visible 	  = TRUE
						istr_mant.Argumento[7] = "2"
					END IF	
					
					is_rut = F_verrut(dw_2.Object.clpr_rut[1], True)
					
					IF is_rut = "" THEN
						dw_2.SetItem(1, "clpr_rut", ls_Null)
					ELSE
						dw_2.SetItem(1, "clpr_rut", is_rut)
						IF NOT ExisteCliente(is_rut) THEN
							dw_2.SetItem(1, "clpr_rut", ls_Null)
						ELSE
							ProdRechazado(is_rut)
						END IF	
					END IF
					
					IF dw_2.Object.odfc_tipret[1] = 1 THEN
                  ActivaDatosRetiro(dw_2.Object.odfc_numero[1])
					END IF
					
				END IF	
				
				IF dw_2.Object.odfc_estado[1] = 0 THEN istr_mant.solo_consulta = TRUE
				
				IF dw_1.RowCount()>0 THEN cb_retiro.Visible = NOT istr_mant.solo_consulta
				
				pb_eli_det.Enabled	=	NOT istr_mant.solo_consulta
				pb_imprimir.Enabled	=	NOT istr_mant.solo_consulta
				
				HabilitaEncab(False)
				pb_ins_det.SetFocus()
			END IF
				dw_2.SetRedraw(True)
		LOOP WHILE respuesta = 1
		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)


end event

event ue_nuevo_detalle();

istr_mant.borra			= False
istr_mant.agrega			= True

OpenWithParm(w_mantencion, istr_mant)

IF dw_1.rowcount()>0 THEN
   pb_eli_det.Enabled	=	TRUE
   pb_grabar.Enabled		=	TRUE
END IF 
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
Integer  li_Valor=1

istr_info.titulo	= "ORDENES DE VENTA"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_spro_ordenventacom"

vinf.dw_1.SetTransObject(sqlca)

li_Valor	=	MessageBox("Emisión Informe","Desea emitir Informe con Valor Unitario",Question!,YesNo!,1)
IF li_Valor = 2 THEN li_Valor = 0

fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[1]),Long(istr_mant.argumento[3]),li_Valor,&
                          Integer(istr_mant.Argumento[7]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
	
	IF dw_2.Object.prod_rechaz[1] = 1 THEN
		//vinf.dw_1.Modify('DataWindow.dw_1.object.autoriza.text = No Autorizado')
		vinf.dw_1.Object.dw_1.Object.autoriza.text = " NO AUTORIZADO"
	ELSE
		//vinf.dw_1.Modify('DataWindow.dw_1.object.autoriza.text = Autorizado')
		vinf.dw_1.Object.dw_1.Object.autoriza.text = " AUTORIZADO"
	END IF	

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar();call super::ue_antesguardar;Long		ll_Fila, ll_NumeroMovto, ll_Secuen, ll_NumeroRetiro, ll_row
Integer	li_Planta, li_TipoMovto, li_TipoRetiro

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_Planta			=	dw_2.Object.plde_codigo[1]
ll_NumeroMovto		=	dw_2.Object.odfc_numero[1]
li_TipoRetiro		= Integer(istr_mant.Argumento[2])

ll_Fila = 1


IF dw_1.RowCount()<=0 THEN 
	Message.DoubleParm = -1
ELSE	
		DO WHILE ll_Fila <= dw_1.RowCount()
			IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = New! THEN
				dw_1.DeleteRow(ll_Fila)
			ELSE
				ll_Fila ++
			END IF
		LOOP
		
		IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
			UPDATE	dba.spro_ordenventacomenca
				SET	odfc_numero = 0
				WHERE	1 = 2;
			
			SELECT	IsNull(Max(odfc_numero), 0) + 1
				INTO	:ll_NumeroMovto
				FROM	dba.spro_ordenventacomenca
				WHERE	plde_codigo	=	:li_Planta;
						
			dw_2.Object.odfc_numero[1]	=	ll_NumeroMovto
			dw_2.Object.odfc_valpar[1] =  1
			
			IF dw_2.Object.odfc_tipret[1] = 1 THEN
				
				UPDATE	dba.spro_ordenretiroventaenc
					SET	oret_numero = 0
					WHERE	1 = 2;
				
				SELECT	IsNull(Max(oret_numero), 0) + 1
					INTO	:ll_NumeroRetiro
					FROM	dba.spro_ordenretiroventaenc
					WHERE	plde_codigo	=	:li_Planta
					  AND tdop_codigo =  :li_TipoRetiro;
					  
				ll_row = ids_retiroenca.InsertRow(0)
				ids_retiroenca.Object.plde_codigo[ll_row] = dw_2.Object.plde_codigo[1]
				ids_retiroenca.Object.tdop_codigo[ll_row] = li_TipoRetiro
				ids_retiroenca.Object.oret_numero[ll_row] = ll_NumeroRetiro
				ids_retiroenca.Object.odfc_numero[ll_row] = ll_NumeroMovto
				ids_retiroenca.Object.oret_fecret[ll_row] = dw_2.Object.odfc_fecham[1]
				ids_retiroenca.Object.oret_estado[ll_row] = 1
			   
				istr_mant.Argumento[6] = string(ll_NumeroRetiro)
				
			END IF
		END IF
		
		istr_Mant.Argumento[3]	=	String(dw_2.Object.odfc_numero[1])
				
		UPDATE	dba.spro_ordenventacomdeta
				SET	odfc_numero = 0
				WHERE	1 = 2;
			
			SELECT	IsNull(Max(ofcd_secuen), 0) + 1
				INTO	:ll_Secuen
				FROM	dba.spro_ordenventacomdeta
				WHERE	plde_codigo	=	:li_Planta
				  AND odfc_numero =  :ll_NumeroMovto;
			
		FOR ll_Fila = 1 TO dw_1.RowCount()
			IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN

				dw_1.Object.plde_codigo[ll_Fila]	=	li_Planta
				dw_1.Object.odfc_numero[ll_Fila]	=	ll_NumeroMovto
				dw_1.Object.ofcd_secuen[ll_Fila]	=	ll_secuen
				
				
				IF dw_2.Object.odfc_tipret[1] = 1 THEN	
					
					ll_NumeroRetiro			=	ids_retiroenca.Object.oret_numero[1]
					
					ll_row=ids_retirodeta.InsertRow(0)
					
					ids_retirodeta.Object.plde_codigo[ll_row] = dw_2.Object.plde_codigo[1]
					ids_retirodeta.Object.tdop_codigo[ll_row] = li_TipoRetiro
					ids_retirodeta.Object.oret_numero[ll_row] = ll_NumeroRetiro
					ids_retirodeta.Object.odre_secuen[ll_row] = ll_secuen
					ids_retirodeta.Object.espe_codigo[ll_row] = dw_1.Object.espe_codigo[ll_fila]
					ids_retirodeta.Object.grva_codigo[ll_row] = dw_1.Object.grva_codigo[ll_fila]
					ids_retirodeta.Object.grva_codsub[ll_row] = dw_1.Object.grva_codsub[ll_fila]
					ids_retirodeta.Object.vari_codigo[ll_row] = dw_1.Object.vari_codigo[ll_fila]
					ids_retirodeta.Object.cate_codigo[ll_row] = dw_1.Object.cate_codigo[ll_fila]
					ids_retirodeta.Object.frio_tipofr[ll_row] = dw_1.Object.frio_tipofr[ll_fila]
					ids_retirodeta.Object.enva_tipoen[ll_row] = dw_1.Object.enva_tipoen[ll_fila]
					ids_retirodeta.Object.enva_codigo[ll_row] = dw_1.Object.enva_codigo[ll_fila]
					ids_retirodeta.Object.odre_gcalib[ll_row] = dw_1.Object.ofcd_gcalib[ll_fila]
					ids_retirodeta.Object.odre_bultos[ll_row] = dw_1.Object.ofcd_bultos[ll_fila]
					ids_retirodeta.Object.odre_tkilos[ll_row] = dw_1.Object.ofcd_tkilos[ll_fila]
				END IF
				
		      ll_secuen++
			END IF
		NEXT

END IF
end event

event ue_borra_detalle();IF dw_1.rowcount() < 2 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(w_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
 IF dw_1.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False
end event

event ue_modifica_detalle();call super::ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(w_mantencion, istr_mant)
END IF
end event

event ue_seleccion();call super::ue_seleccion;Str_Busqueda	lstr_busq

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = ''

OpenWithParm(w_busc_ordenventacomercial, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	
	istr_mant.Argumento[1]	=	lstr_busq.argum[1]
	istr_mant.argumento[3]	=	lstr_busq.argum[2]
	
	This.TriggerEvent("ue_recuperadatos")
	
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_ordenventacomenca
integer x = 27
integer y = 928
integer width = 3145
integer height = 948
string title = "Detalle de Orden de Venta"
string dataobject = "dw_mues_spro_ordenventacomdeta"
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_ordenventacomenca
integer x = 251
integer y = 44
integer width = 2546
integer height = 860
string dataobject = "dw_mant_spro_ordenventacomenca"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Null, ls_Fecha

SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna

	CASE "plde_codigo"
		
		IF Not iuo_planta.existe(integer(data),True,Sqlca) THEN
			This.SetItem(1, "plde_codigo", integer(ls_Null))
		ELSE
			istr_Mant.Argumento[1]	= Data
		END IF
		
	CASE "vend_codigo"
		
		IF Not existevendedor(integer(data)) THEN
			This.SetItem(1, "vend_codigo", integer(ls_Null))
		END IF
		
	CASE "odfc_numero"
		
		IF ExisteMovimiento(Long(Data)) THEN
			istr_Mant.Argumento[3]	=	Data
			Parent.TriggerEvent("ue_recuperadatos")
		ELSE
			This.SetItem(1, ls_Columna, Long(ls_Null))
			RETURN 1
		END IF

	CASE "odfc_fecham"
		ls_Fecha	=	Data
		This.SetItem(1, ls_Columna, Date(Mid(ls_Fecha,1,10)))
      istr_Mant.Argumento[5]=Mid(Data,1,10)
		
	CASE "clpr_rut"
		is_rut = F_verrut(data, True)
		IF is_rut = "" THEN
			dw_2.SetItem(1, "clpr_rut", ls_Null)
			RETURN 1
		ELSE
			dw_2.SetItem(1, "clpr_rut", is_rut)
			//Existe Cliente
			IF NOT ExisteCliente(is_rut) THEN
				dw_2.SetItem(1, "clpr_rut", ls_Null)
				RETURN 1
			ELSE
				ProdRechazado(is_rut)
			END IF	
		END IF
	CASE "odfc_pesfac"
		IF data = "1" THEN
			dw_1.Object.ofcd_bultos.visible = TRUE
			dw_1.Object.ofcd_tkilos.visible = FALSE
			dw_1.Object.t_kilos.visible 	  = FALSE
			istr_mant.Argumento[7] = "1"
		ELSE
			dw_1.Object.ofcd_bultos.visible = FALSE
			dw_1.Object.ofcd_tkilos.visible = TRUE
			dw_1.Object.t_kilos.visible 	  = TRUE
			istr_mant.Argumento[7] = "2"
		END IF	
	
END CHOOSE

HabilitaIngreso()
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name

	CASE "b_cliente"
		BuscaCliente()

END CHOOSE
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	IF dwo.Name = "clpr_nrorut" THEN
		This.Object.clpr_rut.EditMask.Mask = "XXXXXXXXXX"

		IF is_rut <> "" THEN
			This.SetItem(il_fila, "clpr_rut", String(Double(Mid(is_rut, 1, 9)), "#########") + Mid(is_rut, 10))
		END IF
	ELSE
		This.Object.clpr_rut.EditMask.Mask = "###.###.###-!"
		This.SetItem(il_fila, "clpr_rut", is_rut)
	END IF
END IF
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_ordenventacomenca
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_ordenventacomenca
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_ordenventacomenca
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_ordenventacomenca
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_ordenventacomenca
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_ordenventacomenca
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_ordenventacomenca
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_ordenventacomenca
end type

type cb_retiro from commandbutton within w_maed_spro_ordenventacomenca
boolean visible = false
integer x = 3237
integer y = 1200
integer width = 279
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "O.Retiro"
end type

event clicked;Str_Busqueda lstr_busq

lstr_busq.argum[1] = "OV"
lstr_busq.argum[2] = istr_mant.argumento[3]              //Ord. Venta
lstr_busq.argum[3] = String(dw_2.Object.odfc_tipret[1])
lstr_busq.argum[4] = istr_mant.argumento[6]              //Ord. Retiro Si es Venta Total

OpenSheetWithParm(w_maed_spro_ordenretiroventaenca,lstr_busq,w_main,0,Original!)
end event

