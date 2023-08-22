$PBExportHeader$w_maed_aplic_plaguisidas_productor.srw
forward
global type w_maed_aplic_plaguisidas_productor from w_mant_encab_deta_csd
end type
end forward

global type w_maed_aplic_plaguisidas_productor from w_mant_encab_deta_csd
integer width = 3602
integer height = 2056
string title = "DECLARACION DE APLICACION DE PLAGUICIDAS POR PRODUCTOR"
string menuname = ""
event ue_imprimir ( )
end type
global w_maed_aplic_plaguisidas_productor w_maed_aplic_plaguisidas_productor

type variables
w_mant_deta_aplic_plaguisida_productor iw_mantencion

DataWindowChild	dw_planta, dw_clientes, idwc_especie, idwc_productor, idwc_predio


Boolean	ib_conectado, ib_existe_folioD, ib_conectado2
Integer 	ii_tipoin, ii_secuen, ii_controlaaceso, ii_existe, ii_blockcont, il_cont 
Long     	il_numins, il_Folio

uo_especie				iuo_especie
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string columna)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existepatente (string as_embarque, string as_patente)
public function boolean noexisteproductor (long al_productor)
public function boolean noexistepredio (integer ai_predio, long al_productor)
public function boolean existeregistro ()
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info


lstr_info.titulo	= "APLICACIÓN DECLARACIÓN DE PLAGUICIDAS POR PRODUCTOR"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_aplic_plaguicidas_anexo_prod"
vinf.dw_1.SetTransObject(sqlca)

istr_mant.argumento[3] = String(dw_1.Object.clie_codigo[1])
istr_mant.argumento[1] = String(dw_1.Object.plde_codigo[1])
istr_mant.argumento[4] = String(dw_1.Object.prod_codigo[1])
istr_mant.argumento[2] = String(dw_1.Object.espe_codigo[1])
istr_mant.argumento[5] = String(dw_1.object.decl_fecdec[1])
istr_mant.argumento[6] = String(dw_1.object.decl_predio[1])

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]), Integer(istr_mant.argumento[1]), &
										Long(istr_mant.argumento[4]),Integer(istr_mant.argumento[2]),&
										date(istr_mant.argumento[5]),integer(istr_mant.argumento[6]))
										
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect	=	0
	dw_2.Object.plde_codigo.Protect	=	0
	dw_2.Object.decl_fecdec.Protect	=	0
	dw_2.Object.prod_codigo.Protect	=	0
	dw_2.Object.espe_codigo.Protect	=	0
	dw_2.Object.decl_predio.Protect	=	0
	
	dw_2.Object.espe_codigo.Color	= 0
	dw_2.Object.prod_codigo.Color	= 0
	dw_2.Object.decl_predio.Color 	= 0
	dw_2.Object.clie_codigo.Color 		= 0
	dw_2.Object.plde_codigo.Color 	= 0
	dw_2.Object.decl_fecdec.Color 	= 0
	
	dw_2.Object.espe_codigo.BackGround.Color	= Rgb(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color		= Rgb(255,255,255)
	dw_2.Object.decl_predio.BackGround.Color 		= Rgb(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color 		= Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.decl_fecdec.BackGround.Color 	= Rgb(255,255,255)
	
	dw_2.SetColumn("prod_codigo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect	=	1
	dw_2.Object.plde_codigo.Protect	=	1
	dw_2.Object.decl_fecdec.Protect	=	1
	dw_2.Object.prod_codigo.Protect	=	1
	dw_2.Object.espe_codigo.Protect	=	1
	dw_2.Object.decl_predio.Protect	=	1
	
	dw_2.Object.espe_codigo.Color	= Rgb(255,255,255)
	dw_2.Object.prod_codigo.Color	= Rgb(255,255,255)
	dw_2.Object.decl_predio.Color 	= Rgb(255,255,255)
	dw_2.Object.clie_codigo.Color 		= Rgb(255,255,255)
	dw_2.Object.plde_codigo.Color 	= Rgb(255,255,255)
	dw_2.Object.decl_fecdec.Color 	= Rgb(255,255,255)
	
	dw_2.Object.espe_codigo.BackGround.Color	= 554648127
	dw_2.Object.prod_codigo.BackGround.Color		= 554648127
	dw_2.Object.decl_predio.BackGround.Color 		= 554648127
	dw_2.Object.clie_codigo.BackGround.Color 		= 554648127
	dw_2.Object.plde_codigo.BackGround.Color 	= 554648127
	dw_2.Object.decl_fecdec.BackGround.Color 	= 554648127

END IF

end subroutine

public subroutine habilitaingreso (string columna);Date	ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &		
		IsNull(dw_2.Object.decl_fecdec[1]) OR dw_2.Object.decl_fecdec[1] = ld_fecha OR &
		IsNull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0 OR &
		IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 OR &
		IsNull(dw_2.Object.decl_predio[1]) OR dw_2.Object.decl_predio[1] = 0 OR &		
		IsNull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0 OR &		
		IsNull(dw_2.Object.decl_nomprd[1]) OR dw_2.Object.decl_nomprd[1] = '' OR &		
		IsNull(dw_2.Object.decl_nombre[1]) OR dw_2.Object.decl_nombre[1] = ''  OR &		
		IsNull(dw_2.Object.decl_regpre[1]) OR dw_2.Object.decl_regpre[1] = '' OR &		
		IsNull(dw_2.Object.decl_compre[1]) OR dw_2.Object.decl_compre[1] = ''  OR &		
		IsNull(dw_2.Object.decl_inicos[1]) OR dw_2.Object.decl_inicos[1] = ld_fecha OR &		
		IsNull(dw_2.Object.decl_calida[1]) OR dw_2.Object.decl_calida[1] = '' OR &		
		IsNull(dw_2.Object.decl_finapl[1]) OR dw_2.Object.decl_finapl[1] = ld_fecha THEN
		
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

public function boolean noexisteproductor (long al_productor);String	ls_nombre, ls_embarque, ls_patente
Integer	li_cliente, li_especie, li_planta
Long		ll_productor
	
SELECT	prod_nombre
	INTO	:ls_nombre
	FROM	dbo.productores
	WHERE prod_codigo = :al_productor;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Produtores")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Productor no ha sido creado, Ingrese otro Código.", &
			Exclamation!, OK!)
	RETURN True
ELSE
		
	dw_2.Object.decl_nomprd[1] = ls_nombre
	
	RETURN False
	
END IF


	

end function

public function boolean noexistepredio (integer ai_predio, long al_productor);String	ls_nombre, ls_region, ls_comuna
Integer	li_cliente, li_especie, li_planta, li_region, li_ciudad, li_comuna, li_provincia
Long		ll_productor
	
SELECT	prpr_nombre, regi_codigo,prov_codigo,comu_codigo,ciud_codigo
	INTO	:ls_nombre, :li_region, :li_provincia, :li_comuna, :li_ciudad
	FROM	dbo.spro_prodpredio
	WHERE :al_productor in (-1,prod_codigo) 
	AND	prpr_codigo = :ai_predio;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_prodpredio")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Predio no ha sido creado, Ingrese otro Código.", &
			Exclamation!, OK!)
	RETURN True
ELSE
	
	SELECT regi_nombre
	INTO	:ls_region
	FROM dbo.regiones
	WHERE regi_codigo = :li_region;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla regiones")
		RETURN True
	END IF
	
	SELECT comu_nombre
	INTO	:ls_comuna
	FROM dbo.comunasexp
	WHERE regi_codigo = :li_region
	AND 	ciud_codigo = :li_ciudad
	AND	comu_codigo = :li_comuna
	AND	prov_codigo = :li_provincia;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla comunasexp")
		RETURN True
	END IF
	
	dw_2.Object.decl_nombre[1] = ls_nombre
	dw_2.Object.decl_regpre[1] = ls_region
	dw_2.Object.decl_compre[1] = ls_comuna
	
	RETURN False
END IF


	

end function

public function boolean existeregistro ();String	ls_patente,ls_recibidor, ls_embarque
Integer	li_Cliente, li_especie, li_planta, li_predio
Date		ld_fecdes, ld_fecha
Long		ll_productor

li_Cliente		=	Integer(istr_mant.Argumento[3])
li_planta		=  Integer(istr_mant.Argumento[1])
ll_productor	=	Long(istr_mant.Argumento[4])
li_especie		=	Integer(istr_mant.Argumento[2])
li_predio		=	Integer(istr_mant.Argumento[6])
ld_fecha			=	Date(istr_mant.Argumento[5])

SELECT	count(*)
	INTO	:il_cont
	FROM	dbo.sagdeclaprodenca
	WHERE	clie_codigo	=	:li_Cliente
	AND	plde_codigo	=	:li_planta
   AND   prod_codigo = 	:ll_productor
	AND	espe_codigo =	:li_especie
	AND	decl_fecdec =	:ld_fecha
	AND	decl_predio	=	:li_predio;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla sagdeclaprodencia")
	RETURN True

ELSE
	
	IF il_cont > 0 THEN
		This.TriggerEvent("ue_recuperadatos")
	END IF	
	Return False
END IF
end function

event open;/*
	Argumentos	:	[1]	=	Código de Planta
						[3]	=  Cliente
						[2]	=  especie
						[4]	=	productor
						[5]	=	fecha
						[6]   = 	predio
*/

/* Si Código de parempresa es igual a 1 se efectua la transacción para REBAJE DE EXISTENCIA*/
  SELECT empr_coacce
    INTO :ii_controlaaceso  
    FROM dbo.parempresa  
	 USING sqlca;

//istr_mant.argumento[8] = " "
//x				= 0
//y				= 0
This.Height	= 2500
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

dw_2.GetChild("clie_codigo", dw_clientes)
dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("espe_codigo", idwc_especie)

dw_planta.SetTransObject(sqlca)
idwc_especie.SetTransObject(sqlca)
dw_clientes.SetTransObject(sqlca)

dw_planta.Retrieve(1)
idwc_especie.Retrieve()
dw_clientes.Retrieve()

dw_2.SetItem(1, "clie_codigo",gi_codexport)
istr_mant.argumento[1]= String(gi_codplanta)
istr_mant.argumento[3]= String(gi_codexport)
istr_mant.argumento[5]= String(Date(Today()))
istr_mant.argumento[2]= ''
istr_mant.argumento[4]= ''
istr_mant.argumento[6]= ''

dw_2.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(gi_codexport)

dw_2.GetChild("decl_predio", idwc_predio)
idwc_predio.SetTransObject(sqlca)
idwc_predio.Retrieve(-1)

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
						
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
istr_mant.argumento[4] = String(dw_2.Object.prod_codigo[1])
istr_mant.argumento[5] = String(dw_2.Object.decl_fecdec[1])
istr_mant.argumento[6] = String(dw_2.Object.decl_predio[1])

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
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[3]), Integer(istr_mant.argumento[1]), &
										Long(istr_mant.argumento[4]),Integer(istr_mant.argumento[2]),&
										date(istr_mant.argumento[5]),integer(istr_mant.argumento[6]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[3]), Integer(istr_mant.argumento[1]), &
										Long(istr_mant.argumento[4]),Integer(istr_mant.argumento[2]),&
										date(istr_mant.argumento[5]),integer(istr_mant.argumento[6]))
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

on w_maed_aplic_plaguisidas_productor.create
call super::create
end on

on w_maed_aplic_plaguisidas_productor.destroy
call super::destroy
end on

event ue_nuevo;HabilitaEncab(True)

dw_1.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled		= False
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

istr_mant.argumento[1]= String(gi_codplanta)
istr_mant.argumento[3]= String(gi_codexport)
istr_mant.argumento[5]= String(Date(Today()))
istr_mant.argumento[2]= ''
istr_mant.argumento[4]= ''
istr_mant.argumento[6]= ''

dw_2.SetRedraw(True)

dw_2.SetFocus()
end event

event ue_seleccion;call super::ue_seleccion;Str_busqueda			lstr_busq

lstr_busq.argum[1]	= 	istr_mant.argumento[3]
lstr_busq.argum[2]	= 	istr_mant.argumento[1]

OpenWithParm(w_busc_aplica_plaguisida_productor, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= lstr_busq.argum[2]
	istr_mant.argumento[2]	= lstr_busq.argum[3]
	istr_mant.argumento[3]  = lstr_busq.argum[1]
	istr_mant.argumento[4]  = lstr_busq.argum[4]
	istr_mant.argumento[5]  = lstr_busq.argum[5]
	istr_mant.argumento[6]  = lstr_busq.argum[6]
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
	istr_mant.argumento[4] = String(dw_2.Object.prod_codigo[1])
	istr_mant.argumento[5] = String(dw_2.Object.decl_fecdec[1])
	istr_mant.argumento[6] = String(dw_2.Object.decl_predio[1])

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

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_aplic_plaguisidas_productor
integer x = 59
integer y = 1072
integer width = 3003
integer height = 732
integer taborder = 100
string title = "Detalle de Cajas Productor"
string dataobject = "dw_mues_aplic_plaguisidas_productor"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_aplic_plaguisidas_productor
integer x = 59
integer y = 36
integer width = 2885
integer height = 964
string dataobject = "dw_mant_aplic_plaguisidas_productor"
boolean livescroll = true
end type

event dw_2::itemchanged;call super::itemchanged;String	ls_columna, ls_null, ls_embarque
Date		ld_nula, ld_fecha
Integer	li_Cliente, li_Planta
Long		ll_null, ll_productor

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
			
			dw_2.GetChild("prod_codigo", idwc_productor)
			idwc_productor.SetTransObject(sqlca)
			idwc_productor.Retrieve(Integer(data))
						
		END IF
					
	
	CASE "decl_inicos"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF	
		
	CASE "decl_finapl"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF	
		
	CASE "decl_fecdec"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		ELSE
			istr_mant.argumento[5] = data
			
			IF	IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &		
				IsNull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0 OR &
				IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 OR &
				IsNull(dw_2.Object.decl_predio[1]) OR dw_2.Object.decl_predio[1] = 0 OR &		
				IsNull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0  THEN
			ELSE	
				IF existeregistro() THEN
				END IF	
			END IF
			
		END IF		
	
	CASE "espe_codigo"
		IF iuo_Especie.Existe(Integer(data),True,SQLCA) THEN
			istr_mant.argumento[2] = String(Data)	
			
			IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &		
				IsNull(dw_2.Object.decl_fecdec[1]) OR dw_2.Object.decl_fecdec[1] = ld_fecha OR &
				IsNull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0 OR &
				IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 OR &
				IsNull(dw_2.Object.decl_predio[1]) OR dw_2.Object.decl_predio[1] = 0 THEN
			ELSE	
				IF existeregistro() THEN
				END IF
			END IF
		ELSE
			This.SetItem(row,'espe_codigo',Integer(ls_null))
			RETURN 1
		END IF
		
	CASE "prod_codigo"
		IF NoExisteproductor(Long(data)) THEN
			This.SetItem(il_fila, ls_columna, Long(ls_null))
			RETURN 1
		ELSE
			dw_2.GetChild("decl_predio", idwc_predio)
			idwc_predio.SetTransObject(sqlca)
			idwc_predio.Retrieve(Long(data))
			istr_mant.argumento[4] = data
			IF existeregistro() THEN
			END IF
		END IF
		
	CASE "decl_predio"
		IF NOT IsNull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] <> 0 THEN
			ll_productor = dw_2.Object.Prod_codigo[1]
		ELSE	
			ll_productor = -1
		END IF 	
		
		IF noexistepredio(Long(data),ll_productor) THEN
			This.SetItem(il_fila, ls_columna, Long(ls_null))
			RETURN 1
		ELSE
			istr_mant.argumento[6] = data
			IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &		
				IsNull(dw_2.Object.decl_fecdec[1]) OR dw_2.Object.decl_fecdec[1] = ld_fecha OR &
				IsNull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0 OR &
				IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 OR &
				IsNull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0 THEN
			ELSE	
				IF existeregistro() THEN
				END IF
			END IF
		END IF		
	
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_aplic_plaguisidas_productor
integer x = 3118
integer y = 264
end type

event pb_nuevo::clicked;call super::clicked;ib_existe_folioD	=	False
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_aplic_plaguisidas_productor
integer x = 3118
integer y = 492
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_aplic_plaguisidas_productor
integer x = 3118
integer y = 708
end type

event pb_grabar::clicked;call super::clicked;//ib_existe_folioD	=	True
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_aplic_plaguisidas_productor
integer x = 3118
integer y = 940
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_aplic_plaguisidas_productor
integer x = 3118
integer y = 1168
end type

event pb_salir::clicked;
Close(Parent)
end event

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_aplic_plaguisidas_productor
integer x = 3118
integer y = 1496
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_aplic_plaguisidas_productor
integer x = 3118
integer y = 1672
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_aplic_plaguisidas_productor
integer x = 3118
integer y = 84
end type

