$PBExportHeader$w_maed_aplic_plaguisidas.srw
forward
global type w_maed_aplic_plaguisidas from w_mant_encab_deta_csd
end type
end forward

global type w_maed_aplic_plaguisidas from w_mant_encab_deta_csd
integer width = 3566
integer height = 2256
string title = "DECLARACION DE APLICACION DE PLAGUICIDAS"
string menuname = ""
event ue_imprimir ( )
end type
global w_maed_aplic_plaguisidas w_maed_aplic_plaguisidas

type variables
w_mant_deta_aplic_plaguisida iw_mantencion

DataWindowChild	dw_planta, dw_clientes, idwc_especie, idwc_productor


Boolean		ib_conectado, ib_existe_folioD, ib_conectado2
Integer     ii_tipoin, ii_secuen, ii_controlaaceso, ii_existe, ii_blockcont 
Long        il_numins, il_Folio


uo_patente				iuo_patente
uo_especie				iuo_especie
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string columna)
public function boolean noexisteembarque (string as_embarque)
public subroutine buscaembarque ()
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existepatente (string as_embarque, string as_patente)
public function boolean buscacajasdetalle (string as_embarque)
public function boolean cargaembarque (string as_embarque)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

SetPointer(HourGlass!)

istr_mant.argumento[3] = String(dw_1.Object.clie_codigo[1])
istr_mant.argumento[1] = String(dw_1.Object.plde_codigo[1])
istr_mant.argumento[4] = dw_1.Object.embq_codigo[1]
istr_mant.argumento[2] = String(dw_1.Object.espe_codigo[1])
istr_mant.argumento[5] = dw_1.object.decl_patent[1]


OpenWithParm(w_info_selec_aplic_plaguicidas, istr_mant)
end event

public subroutine habilitaencab (boolean habilita);Return
end subroutine

public subroutine habilitaingreso (string columna);Date	ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &		
		IsNull(dw_2.Object.decl_fecdes[1]) OR dw_2.Object.decl_fecdes[1] = ld_fecha OR &
		IsNull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] = '' OR &
		IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 OR &
		IsNull(dw_2.Object.decl_tcajas[1]) OR dw_2.Object.decl_tcajas[1] = 0 OR &		
		IsNull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0 OR &		
		IsNull(dw_2.Object.decl_patent[1]) OR dw_2.Object.decl_patent[1] = '' OR &		
		IsNull(dw_2.Object.decl_ultapl[1]) OR dw_2.Object.decl_ultapl[1] = ld_fecha  OR &		
		IsNull(dw_2.Object.decl_contra[1]) OR dw_2.Object.decl_contra[1] = '' OR &		
		IsNull(dw_2.Object.decl_plasag[1]) OR dw_2.Object.decl_plasag[1] = 0  OR &	
		IsNull(dw_2.Object.decl_ultnom[1]) OR dw_2.Object.decl_ultnom[1] = ''  OR &		
		IsNull(dw_2.Object.decl_fecmov[1]) OR dw_2.Object.decl_fecmov[1] = ld_fecha THEN
		
		lb_estado = False
	ELSE
		lb_estado = True
			
	END IF
END IF

//pb_grabar.Enabled		=	lb_estado
pb_ins_det.Enabled  	= 	lb_estado
pb_eli_det.Enabled  	= 	lb_estado
//pb_imprimir.Enabled 	= 	lb_estado
end subroutine

public function boolean noexisteembarque (string as_embarque);String	ls_patente,ls_recibidor, ls_embarque
Integer	li_Cliente, li_especie, li_planta, li_cont
Date		ld_fecdes
Long		ll_nrosag

li_Cliente	=	Integer(istr_mant.Argumento[3])
li_planta	=  Integer(istr_mant.Argumento[1])

SELECT	reci_nombre
	INTO	:ls_recibidor
	FROM	dbo.embarqueprod as em, dbo.recibidores as re
	WHERE	em.clie_codigo	=	:li_Cliente
	AND	em.embq_codigo	=	:as_Embarque
   AND   em.reci_codigo = re.reci_codigo;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embarqueprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embarque no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN True
ELSE
	
	
	SELECT max(defe_fecdes),defe_patent,defe_especi, defe_plasag
	INTO	:ld_fecdes,:ls_patente,:li_especie, :ll_nrosag
	FROM dbo.despafrigoen
	WHERE clie_codigo	=	:li_Cliente
	AND	embq_codigo	=	:as_Embarque
	AND	plde_codigo = 	:li_planta
	GROUP BY defe_patent,defe_especi,defe_plasag;
	
	
	dw_2.SetItem(il_fila, "espe_codigo", li_especie)
	dw_2.SetItem(il_fila, "decl_patent", ls_patente)
	dw_2.SetItem(il_fila, "decl_fecdes", ld_fecdes)
	dw_2.Object.decl_plasag[il_fila] = ll_nrosag
	
	istr_mant.Argumento[4]	=	as_Embarque
	
	IF buscacajasdetalle(as_Embarque) THEN
	END IF	
	//istr_mant.Argumento[7]	=	String(li_Destino)
	
	istr_mant.argumento[3] = String(dw_2.Object.clie_codigo[1])
	istr_mant.argumento[1] = String(dw_2.Object.plde_codigo[1])
	istr_mant.argumento[4] = as_Embarque
	istr_mant.argumento[2] = String(dw_2.Object.espe_codigo[1])
	istr_mant.argumento[5] = dw_2.object.decl_patent[1]
	
	li_cliente 	= dw_2.Object.clie_codigo[1]
	li_planta 	= dw_2.Object.plde_codigo[1]
	ls_embarque = dw_2.Object.embq_codigo[1]
	li_especie 	= dw_2.Object.espe_codigo[1]
	ls_patente 	= dw_2.object.decl_patent[1]
	
	SELECT count(*)
	INTO :li_cont
	FROM dbo.sagdeclaraenca
	WHERE clie_codigo = :li_cliente
	AND	plde_codigo = :li_planta
	AND	embq_codigo = :as_Embarque
	AND	espe_codigo = :li_especie
	AND	decl_patent = :ls_patente;
	
	IF li_cont > 0 THEN
		This.TriggerEvent("ue_recuperadatos")
	END IF	
	
	RETURN False
END IF
end function

public subroutine buscaembarque ();Str_busqueda	lstr_busq

dw_2.Modify("buscaembarque.border = 0")
dw_2.Modify("buscaembarque.border = 5")

lstr_busq.argum[1]	=	istr_mant.argumento[3] // Cliente.

OpenWithParm(w_busc_embarques, lstr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	dw_2.SetItem(il_fila, "embq_codigo", istr_busq.argum[1])
//	dw_2.SetItem(il_fila, "embq_nomnav", istr_busq.argum[2])
//	dw_2.SetItem(il_fila, "puer_codigo", Integer(istr_busq.argum[4]))
   	
	noexisteembarque(istr_busq.argum[1])
	
//	istr_mant.argumento[7]	=	istr_busq.argum[5]
ELSE
	dw_2.SetColumn("embq_codigo")
	dw_2.SetFocus()
END IF

dw_2.Modify("buscaembarque.border = 0")
dw_2.Modify("buscaembarque.border = 6")

end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE

	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
		
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
				
			END IF	
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean existepatente (string as_embarque, string as_patente);String	ls_patente,ls_recibidor
Integer	li_Cliente, li_especie, li_planta
Date		ld_fecdes

li_Cliente	=	Integer(istr_mant.Argumento[3])
li_planta	=  Integer(istr_mant.Argumento[1])


	SELECT max(defe_fecdes),defe_patent,defe_especi
	INTO	:ld_fecdes,:ls_patente,:li_especie
	FROM dbo.despafrigoen
	WHERE clie_codigo	=	:li_Cliente
	AND	embq_codigo	=	:as_Embarque
	AND	plde_codigo = 	:li_planta
	AND	defe_patent =  :as_patente
	GROUP BY defe_patent,defe_especi;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla despafrigoen")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Número de patente no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean buscacajasdetalle (string as_embarque);Long		ll_cajas
Integer	li_cliente, li_planta

li_Cliente	=	Integer(istr_mant.Argumento[3])
li_planta	=  Integer(istr_mant.Argumento[1])

SELECT sum(pan.pafr_ccajas)
INTO	:ll_cajas
FROM dbo.despafrigoen as enc, dbo.despafrigode as det, dbo.palletfruta as pan
WHERE enc.clie_codigo =	:li_Cliente
AND	enc.embq_codigo =	:as_Embarque
AND	enc.plde_codigo = :li_planta
AND	enc.clie_codigo = det.clie_codigo
AND	enc.plde_codigo = det.plde_codigo
AND	enc.defe_numero = det.defe_numero
AND	enc.clie_codigo = pan.clie_codigo
AND	enc.plde_codigo = pan.plde_codigo
AND	det.paen_numero = pan.paen_numero;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletfruta")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","No Existe Detalle en Palletfruta. Ingrese Otra.", Exclamation!, Ok!)
	RETURN True
ELSE
	dw_2.Object.decl_tcajas[1] = ll_cajas
	RETURN False
END IF
end function

public function boolean cargaembarque (string as_embarque);String	ls_patente,ls_recibidor, ls_embarque
Integer	li_Cliente, li_especie, li_planta, li_cont
Date		ld_fecdes
Long		ll_nrosag

li_Cliente	=	Integer(istr_mant.Argumento[3])
li_planta	=  Integer(istr_mant.Argumento[1])

SELECT	reci_nombre
	INTO	:ls_recibidor
	FROM	dbo.embarqueprod as em, dbo.recibidores as re
	WHERE	em.clie_codigo	=	:li_Cliente
	AND	em.embq_codigo	=	:as_Embarque
   AND   em.reci_codigo = re.reci_codigo;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embarqueprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embarque no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN True
ELSE
		
	SELECT max(defe_fecdes),defe_patent,defe_especi, defe_plasag
	INTO	:ld_fecdes,:ls_patente,:li_especie, :ll_nrosag
	FROM dbo.despafrigoen
	WHERE clie_codigo	=	:li_Cliente
	AND	embq_codigo	=	:as_Embarque
	AND	plde_codigo = 	:li_planta
	GROUP BY defe_patent,defe_especi,defe_plasag;
		
	dw_2.SetItem(il_fila, "espe_codigo", li_especie)
	dw_2.SetItem(il_fila, "decl_patent", ls_patente)
	dw_2.SetItem(il_fila, "decl_fecdes", ld_fecdes)
	dw_2.Object.decl_plasag[il_fila] = ll_nrosag
	
	istr_mant.Argumento[4]	=	as_Embarque
	
	IF buscacajasdetalle(as_Embarque) THEN
	END IF	
	//istr_mant.Argumento[7]	=	String(li_Destino)
		
	RETURN False
END IF
end function

event open;/*
	Argumentos	:	[1]	=	Código de Planta
						[3]	=  Cliente
						[2]	=  especie
						[4]	=	Embarque
						[5]	=	patente
						[6]   = 	Total cajas
*/

/* Si Código de parempresa es igual a 1 se efectua la transacción para REBAJE DE EXISTENCIA*/
  SELECT empr_coacce
    INTO :ii_controlaaceso  
    FROM dbo.parempresa  
	 USING sqlca;

//istr_mant.argumento[8] = " "
//x				= 0
//y				= 0
//This.Height	= 2320
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("espe_codigo", idwc_especie)

dw_planta.SetTransObject(sqlca)
idwc_especie.SetTransObject(sqlca)

dw_planta.Retrieve(1)
idwc_especie.Retrieve()

dw_2.SetItem(1, "clie_codigo",gi_codexport)
istr_mant.argumento[1]= String(gi_codplanta)
istr_mant.argumento[3]= String(gi_codexport)

dw_1.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(gi_codexport)

dw_2.Object.decl_fecmov[1] = Date(Today())

pb_nuevo.PostEvent(Clicked!)

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"
GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
						

iuo_patente				=	CREATE	uo_patente
iuo_Especie				=	CREATE uo_Especie




end event

event ue_borra_detalle;call super::ue_borra_detalle;SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

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

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

Integer	li_cajas

istr_mant.argumento[2] = String(dw_2.Object.espe_codigo[1])
istr_mant.argumento[1] = String(dw_2.Object.plde_codigo[1])
istr_mant.argumento[3] = String(dw_2.Object.clie_codigo[1])
istr_mant.argumento[4] = String(dw_2.Object.embq_codigo[1])
istr_mant.argumento[5] = String(dw_2.Object.decl_patent[1])
istr_mant.argumento[6] = String(dw_2.Object.decl_tcajas[1])

OpenWithParm(iw_mantencion, istr_mant)
	
IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[3]), &
										Integer(istr_mant.argumento[1]), &
										(istr_mant.argumento[4]),Integer(istr_mant.argumento[2]),(istr_mant.argumento[5]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[3]), &
										Integer(istr_mant.argumento[1]), &
										(istr_mant.argumento[4]),Integer(istr_mant.argumento[2]),(istr_mant.argumento[5]))
										
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_imprimir.Enabled	= True
				
				dw_2.SetTabOrder("clie_codigo",0)
				dw_2.SetTabOrder("plde_codigo",0)
				
				istr_mant.solo_consulta 	=	False
				pb_eli_det.Enabled		=	True
				pb_ins_det.Enabled		=	True
				pb_grabar.Enabled			=	True
				pb_eliminar.Enabled		=	True
												
				IF ll_fila_d > 0 THEN
					pb_imprimir.Enabled	= True
					
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

end event

on w_maed_aplic_plaguisidas.create
call super::create
end on

on w_maed_aplic_plaguisidas.destroy
call super::destroy
end on

event ue_nuevo;HabilitaEncab(True)

dw_1.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled			= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False
dw_2.Enabled				= True

dw_2.SetTabOrder("clie_codigo",1)
dw_2.SetTabOrder("plde_codigo",2)
				
dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)
dw_2.SetItem(1, "plde_codigo", gi_codplanta)
dw_2.Object.decl_fecmov[1] = Date(Today())

dw_2.SetRedraw(True)

dw_2.SetFocus()
end event

event ue_seleccion;call super::ue_seleccion;Str_busqueda			lstr_busq

lstr_busq.argum[1]	= 	istr_mant.argumento[3]
lstr_busq.argum[2]	= 	istr_mant.argumento[1]

OpenWithParm(w_busc_aplica_plaguisida, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= lstr_busq.argum[2]
	istr_mant.argumento[2]	= lstr_busq.argum[3]
	istr_mant.argumento[3]  = lstr_busq.argum[1]
	istr_mant.argumento[4]  = lstr_busq.argum[4]
	istr_mant.argumento[5]  = lstr_busq.argum[5]
	ib_existe_folioD			=	True
	This.TriggerEvent("ue_recuperadatos")
	
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega			= False
	istr_mant.borra			= False
	istr_mant.argumento[2] = String(dw_2.Object.espe_codigo[1])
	istr_mant.argumento[1] = String(dw_2.Object.plde_codigo[1])
	istr_mant.argumento[3] = String(dw_2.Object.clie_codigo[1])
	istr_mant.argumento[4] = String(dw_2.Object.embq_codigo[1])
	istr_mant.argumento[5] = String(dw_2.Object.decl_patent[1])
	istr_mant.argumento[6] = String(dw_2.Object.decl_tcajas[1])

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
//		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
		pb_ins_det.Enabled	=	True
		pb_eli_det.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_ins_det.Enabled	=	False
	ELSE
		pb_ins_det.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_aplic_plaguisidas
integer x = 55
integer y = 1524
integer width = 2939
integer height = 564
integer taborder = 100
string title = "Detalle de Cajas Productor"
string dataobject = "dw_mues_aplic_plaguisidas"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_aplic_plaguisidas
integer x = 59
integer y = 0
integer width = 2885
integer height = 1480
string dataobject = "dw_mant_aplic_plaguisidas"
end type

event dw_2::itemchanged;call super::itemchanged;String	ls_columna, ls_null, ls_embarque
Date		ld_nula
Integer	li_Cliente, li_Planta
Long		ll_null

SetNull(ll_null)
SetNull(ls_null)
SetNull(ld_nula)
ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		istr_mant.argumento[3]	= data
		IF F_ValidaCliente(Integer(data)) = False THEN
			dw_2.SetItem(il_fila, "clie_codigo", gi_codexport)
			RETURN 1
		ELSE
			dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)
			
			dw_1.GetChild("prod_codigo", idwc_productor)
			idwc_productor.SetTransObject(sqlca)
			idwc_productor.Retrieve(Integer(data))
						
			dw_2.SetItem(1, "embq_codigo", ls_null)
		END IF
					
	
	CASE "embq_codigo"
		IF NoExisteEmbarque(data) THEN
			This.Object.embq_codigo[row]	=	ls_null
			RETURN 1
		END IF
	
	CASE "decl_fecdes"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF	
	
CASE "espe_codigo"
	IF iuo_Especie.Existe(Integer(data),True,SQLCA) THEN
		istr_mant.argumento[2] = String(Data)	
		
	ELSE
		This.SetItem(row,'espe_codigo',Integer(ls_null))
		RETURN 1
	END IF
	
CASE "decl_patent"
	
	IF NOT IsNull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] <> '' THEN
		ls_embarque = dw_2.Object.embq_codigo[row]
		IF existepatente(ls_embarque,data) THEN
			This.Object.decl_patent[row]	=	ls_null
			RETURN 1
		ELSE
			Parent.TriggerEvent("ue_nuevo")
					
			IF cargaembarque(ls_embarque) THEN
				This.Object.embq_codigo[row]	=	ls_null
				RETURN 1
			ELSE
				This.Object.decl_patent[row]	=	data
				This.Object.embq_codigo[row]	=	ls_embarque
			END IF
			
		END IF
	ELSE
		MessageBox("Atención","Ingrese Embarque para Seleccionar Patente.", Exclamation!, Ok!)
		This.Object.decl_patent[row]	=	ls_null
	END IF	
	
	

		
					
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

event dw_2::clicked;call super::clicked;CHOOSE CASE dwo.name
		
	CASE "buscaembarque"
		buscaembarque()
		
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_aplic_plaguisidas
integer x = 3118
integer y = 372
end type

event pb_nuevo::clicked;call super::clicked;ib_existe_folioD	=	False
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_aplic_plaguisidas
integer x = 3118
integer y = 600
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_aplic_plaguisidas
integer x = 3118
integer y = 816
end type

event pb_grabar::clicked;call super::clicked;//ib_existe_folioD	=	True
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_aplic_plaguisidas
integer x = 3118
integer y = 1048
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_aplic_plaguisidas
integer x = 3118
integer y = 1276
end type

event pb_salir::clicked;
Close(Parent)
end event

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_aplic_plaguisidas
integer x = 3118
integer y = 1700
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_aplic_plaguisidas
integer x = 3118
integer y = 1876
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_aplic_plaguisidas
integer x = 3118
integer y = 192
end type

