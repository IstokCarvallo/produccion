$PBExportHeader$w_mant_planillasmodificadas.srw
$PBExportComments$Mantenedor Directo de Tipos de Inspección.
forward
global type w_mant_planillasmodificadas from w_mant_directo
end type
end forward

global type w_mant_planillasmodificadas from w_mant_directo
integer width = 3040
integer height = 1144
string title = "MANTENCION PLANILLAS MODIFICADAS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
end type
global w_mant_planillasmodificadas w_mant_planillasmodificadas

type variables
DataWindowChild					idwc_zona, idwc_variedad, idwc_packing, idwc_embalaje, idwc_calibre, idwc_Productor


uo_cliente							iuo_cliente
uo_plantadesp						iuo_planta

Integer								ii_cliente, ii_planta, ii_especie
Long									il_planilla
end variables

forward prototypes
public function boolean duplicado (string valor)
protected function boolean wf_actualiza_db ()
public subroutine buscaloteuva ()
public subroutine buscaloteespe ()
public function boolean existeplanillaespecie (long al_codigo)
public function boolean buscaplanillauva (long al_codigo)
public subroutine validanumero ()
end prototypes

public function boolean duplicado (string valor);//Long		ll_fila
//
//ll_fila	= dw_1.Find("ccti_codigo = " + valor, + &
//							1, dw_1.RowCount())
//
//IF ll_fila > 0 and ll_fila <> il_fila THEN
//	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
//	RETURN True
//ELSE
	RETURN False
//END IF
//
end function

protected function boolean wf_actualiza_db ();Return True
end function

public subroutine buscaloteuva ();Integer	li_Zona, li_Planta, respuesta
Long		ll_Fila,ll_Lote,ll_productor

li_Zona      = dw_1.Object.zona_codigo[1]
li_Planta    = dw_1.Object.plde_codigo[1]
ll_Productor = dw_1.Object.prod_codigo[1]

IF (Isnull(li_zona) OR li_zona = 0) OR (Isnull(li_planta) OR li_planta = 0) OR (Isnull(ll_productor) OR ll_productor=0) THEN 
   Messagebox("Atención","Falta Ingresar Planilla")
	dw_1.setfocus()
	RETURN
ELSE
	istr_busq.argum[1]  = String(dw_1.object.plde_codigo[1])
	istr_busq.argum[2]  = String(dw_1.object.prod_codigo[1])
	istr_busq.argum[3]  = String(dw_1.object.clie_codigo[1])
   istr_busq.argum[12] = String(dw_1.Object.vari_codigo[1])
	istr_busq.argum[13] = String(dw_1.Object.vaca_calibr[1])
	istr_busq.argum[14] = String(dw_1.Object.cclo_fecemb[1])
	
	OpenWithParm(w_busc_lotes_cuantitativas, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
	
	IF istr_busq.argum[2] <> "" THEN
		dw_1.Object.espe_codigo[1] =	Integer(istr_busq.argum[3])
		dw_1.Object.vari_codigo[1] =	Integer(istr_busq.argum[4])
		li_Planta						=	Integer(istr_busq.argum[10])
		ll_Lote							=	Long(istr_busq.argum[11])
		istr_mant.Argumento[1]		=	String(istr_busq.argum[11])		
		istr_mant.argumento[9]		=	istr_busq.argum[3]
		istr_mant.argumento[10]		=	istr_busq.argum[4]
		istr_mant.argumento[8]		=	istr_busq.argum[2]	
		istr_mant.argumento[17]		=	istr_busq.argum[5]
		dw_1.Object.nuevo[1] = ll_lote	
	END IF
END IF

end subroutine

public subroutine buscaloteespe ();Integer	 li_Planta, li_Especie, li_zona, li_cont
Long      ll_fila, ll_lote
String    ls_mensaje, ls_colu[]

IF (Isnull(dw_1.Object.prod_codigo[1]) OR dw_1.Object.prod_codigo[1] = 0) THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nProductor"
	ls_colu[li_cont]	= "prod_codigo"
END IF

IF (Isnull(dw_1.Object.vari_codigo[1]) OR dw_1.Object.vari_codigo[1] = 0) THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nVariedad"
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF (Isnull(dw_1.Object.plde_codigo[1]) OR dw_1.Object.plde_codigo[1] = 0) THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nPlanta"
	ls_colu[li_cont]	= "plde_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de Nº Planilla.", StopSign!, Ok!)
	dw_1.SetFocus()
	Message.DoubleParm = -1
ELSE
	
	istr_busq.argum[1]  = String(dw_1.object.plde_codigo[1])
	istr_busq.argum[2]  = String(dw_1.object.prod_codigo[1])
	istr_busq.argum[4]  = String(dw_1.Object.zona_codigo[1])
	istr_busq.argum[5]  = String(gi_codexport)
	istr_busq.argum[7]  = String(dw_1.Object.emba_codigo[1])
	istr_busq.argum[9]  = String(dw_1.Object.cclo_fecemb[1])
	istr_busq.argum[6]  = String(dw_1.Object.vari_codigo[1])
	istr_busq.argum[12] = String(dw_1.Object.vaca_calibr[1])
	istr_busq.argum[13] = String(ii_especie)
	
	OpenWithParm(w_busc_verificacionlotes, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
		
	IF istr_busq.argum[4] <> "" THEN
		li_Planta						=	Integer(istr_busq.argum[1])
		ll_Lote							=	Integer(istr_busq.argum[3])
		dw_1.Object.plde_codigo[1] =	Integer(istr_busq.argum[1])
		dw_1.Object.vari_codigo[1] =	Integer(istr_busq.argum[6])
		//dw_1.Object.cclo_numero[1] =  Long(istr_busq.argum[3])
		dw_1.Object.prod_codigo[1] =  Long(istr_busq.argum[2])
				 		
		istr_mant.Argumento[1]		=	istr_busq.argum[3]
		istr_mant.argumento[4]		=	istr_busq.argum[1]
		istr_mant.argumento[8]		=	istr_busq.argum[2]
		istr_mant.argumento[10]		=	istr_busq.argum[6]
		istr_mant.argumento[12]		=	istr_busq.argum[7]
		istr_mant.argumento[13]		=	istr_busq.argum[10]
		istr_mant.argumento[16]		=	istr_busq.argum[11]
		istr_mant.argumento[17]		=	istr_busq.argum[8]
		istr_mant.argumento[18]		=	istr_busq.argum[9]
		dw_1.Object.nuevo[1] = ll_lote
	END IF
END IF

end subroutine

public function boolean existeplanillaespecie (long al_codigo);Long li_count
 
 SELECT Count(*)
 INTO :li_count
 FROM dbo.ctlcallotes as cl1, dbo.ctlcalplaniverifienca as pvu  
WHERE cl1.clie_codigo = pvu.clie_codigo  and  
	cl1.plde_codigo = pvu.plde_codigo  and  
	cl1.cclo_numero = pvu.cclo_numero  and  
	cl1.prod_codigo = pvu.prod_codigo  and  
	cl1.espe_codigo = pvu.espe_codigo  and  
	cl1.vari_codigo = pvu.vari_codigo  and  
	cl1.clie_codigo = :ii_cliente  AND  
	cl1.plde_codigo = :ii_planta  AND  
	pvu.ccpv_numero = :al_codigo  AND  
	cl1.espe_codigo = :ii_especie;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla ctlcalplaniverifienca")
	RETURN True
ELSEIF li_count	=	0 THEN
	
	Messagebox("Atención","Número de Planilla No Existe",Exclamation!)

	RETURN True
ELSE	
	RETURN False
END IF

			

end function

public function boolean buscaplanillauva (long al_codigo);Long ll_count
  
SELECT count(*)
INTO :ll_count
 FROM dbo.ctlcallotes as cl1,   
dbo.ctlcalplacuaninspuvaenc as pvu  
WHERE  cl1.clie_codigo = pvu.clie_codigo  and  
 cl1.plde_codigo = pvu.plde_codigo  and  
 cl1.cclo_numero = pvu.cclo_numero  and  
 cl1.prod_codigo = pvu.prod_codigo  and  
 cl1.espe_codigo = pvu.espe_codigo  and  
 cl1.vari_codigo = pvu.vari_codigo  and  
 cl1.clie_codigo = :ii_cliente  AND  
 cl1.plde_codigo = :ii_planta  AND  
 pvu.ccpe_numero = :al_codigo  AND  
 cl1.espe_codigo = :ii_especie;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla ctlcalplaniverifienca")
	RETURN True
ELSEIF ll_count	=	0 THEN
	
	Messagebox("Atención","Número de Planilla No Existe",Exclamation!)

	RETURN True
ELSE	
	RETURN False
END IF

end function

public subroutine validanumero ();
end subroutine

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
Integer li_productor

DO
	ll_fila	= dw_1.Retrieve(ii_cliente, ii_planta, il_planilla, ii_especie)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetFocus()
		il_fila						= 1
		pb_grabar.Enabled	= True
		
		li_productor = dw_1.Object.plde_codpak[1]
		
		dw_1.GetChild("plde_codpak", idwc_packing)
		idwc_packing.SetTransObject(sqlca)
		idwc_packing.Retrieve(li_productor)
		
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;x				= 0
y				= 0
//This.Width	= dw_1.width + 540
//This.Height	= 1993
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

ii_especie	= Integer(Message.StringParm)

iuo_cliente			=	Create uo_cliente
iuo_planta			=	Create uo_plantadesp

IF ii_especie = 11 THEN dw_1.DataObject = "dw_mant_planillasmodificadas_uvas"


dw_1.GetChild("prod_codigo", idwc_Productor)
idwc_Productor.SetTransObject(sqlca)
idwc_Productor.Retrieve(-1)

dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.Retrieve(ii_especie)

dw_1.GetChild("plde_codpak", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(-1)

dw_1.GetChild("emba_codigo", idwc_embalaje)
idwc_embalaje.SetTransObject(sqlca)
idwc_embalaje.Retrieve()

dw_1.GetChild("vaca_calibr", idwc_calibre)
idwc_calibre.SetTransObject(sqlca)
idwc_calibre.Retrieve()

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
//dw_1.SetRowFocusIndicator(Hand!)
//dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
dw_1.InsertRow(0)

dw_1.Object.plde_codigo[1]	=	gi_codPlanta
dw_1.Object.espe_codigo[1]	=	ii_especie
dw_1.Object.clie_codigo[1]	=	gi_CodExport

ii_cliente = gi_CodExport
ii_planta  = gi_codPlanta
end event

on w_mant_planillasmodificadas.create
call super::create
end on

on w_mant_planillasmodificadas.destroy
call super::destroy
end on

event ue_validaregistro();call super::ue_validaregistro;//Integer	li_cont
//String	ls_mensaje, ls_colu[]
//
//IF Isnull(dw_1.Object.ccti_codigo[il_fila]) OR dw_1.Object.ccti_codigo[il_fila] = 0 THEN
//	li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nTipo de Inspección"
//	ls_colu[li_cont]	= "ccti_codigo"
//END IF
//
//IF Isnull(dw_1.Object.ccti_descrip[il_fila]) OR dw_1.Object.ccti_descrip[il_fila] = "" THEN
//	li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nNombre de Inspección"
//	ls_colu[li_cont]	= "ccti_descrip"
//END IF
//
//
//IF li_cont > 0 THEN
//	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
//	dw_1.SetColumn(ls_colu[1])
//	dw_1.SetFocus()
//	Message.DoubleParm = -1
//END IF
end event

event ue_imprimir;//SetPointer(HourGlass!)
//
//Integer	li_zona
//Long		fila
//str_info	lstr_info
//
//lstr_info.titulo	= "MAESTRO DE TIPOS DE INSPECCION"
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
//vinf.dw_1.DataObject = "dw_info_ctlcaltipoinspeccion"
//vinf.dw_1.SetTransObject(sqlca)
//
//fila = vinf.dw_1.Retrieve()
//
//IF fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF fila = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Sort()
//	IF gs_Ambiente <> 'Windows' THEN
//		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
//
//	END IF
//END IF
//
//SetPointer(Arrow!)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long 		ll_Planilla, ll_lote, ll_lote_ant

IF Isnull(dw_1.Object.nuevo[1]) OR dw_1.Object.nuevo[1] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNuevo Lote"
	ls_colu[li_cont]	= "nuevo"
END IF

IF ii_especie = 11 THEN
	IF Isnull(dw_1.Object.ccpe_numero[1]) OR dw_1.Object.ccpe_numero[1] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nPlanilla de Inspección"
		ls_colu[li_cont]	= "ccpe_numero"
	ELSE
		ll_Planilla = dw_1.Object.ccpe_numero[1]
	END IF
ELSE
	IF Isnull(dw_1.Object.ccpv_numero[il_fila]) OR dw_1.Object.ccpv_numero[1] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nPlanilla de Inspección"
		ls_colu[li_cont]	= "ccpv_numero"
	ELSE
		ll_Planilla = dw_1.Object.ccpv_numero[1]
	END IF
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
	Return
END IF

ll_lote		= dw_1.Object.cclo_numero[1]
ll_lote_ant	= dw_1.Object.nuevo[1]
	
IF ii_especie = 11 THEN	
	DECLARE Modificaplanilla PROCEDURE FOR dbo.CtlCal_modifica_lote_planillauvas 
				
				@Planta 		=:ii_planta,
				@cliente  	=:ii_cliente,
				@planilla 	=:il_planilla,
				@lote 		=:ll_lote,
				@lote_nuevo =:ll_lote_ant;
				
	EXECUTE Modificaplanilla;
	
	IF sqlca.sqlcode < 0 THEN
		F_ErrorBaseDatos(sqlca,"El proceso de Actualización" +&
			  " No Se ejecutó Exitosamente")
		Message.DoubleParm = -1
		Return 
	ELSE
		Close Modificaplanilla;
	
		MessageBox("Atención", "El proceso de Actualización" +&
						" se ejecutó Exitosamente")
	END IF
ELSE
	DECLARE Modificaplanillaespe PROCEDURE FOR dbo.CtlCal_modifica_lote_planillaespecie 
				
				@Planta 		=:ii_planta,
				@cliente  	=:ii_cliente,
				@planilla 	=:il_planilla,
				@lote 		=:ll_lote,
				@lote_nuevo =:ll_lote_ant;
				
	EXECUTE Modificaplanillaespe;
	
	IF sqlca.sqlcode < 0 THEN
		F_ErrorBaseDatos(sqlca,"El proceso de Actualización" +&
			  " No Se ejecutó Exitosamente")
		Message.DoubleParm = -1
		Return 
	ELSE
		Close Modificaplanillaespe;
	
		MessageBox("Atención", "El proceso de Actualización" +&
						" se ejecutó Exitosamente")
	END IF
END IF




end event

event resize;//Integer		li_posi_y, li_objeto
//
//dw_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_1.y - 75)
//
//dw_1.x					= 78
//st_encabe.width		= dw_1.width
//
//gb_1.x 					= This.WorkSpaceWidth() - 351
//gb_1.y 					= 33
//gb_1.width				= 275
//gb_1.height				= 180 * 1 + 97 /*  (1 Botón)  */
//
//pb_lectura.x			= This.WorkSpaceWidth() - 292
//pb_lectura.y			= gb_1.y + 88
//pb_lectura.width		= 156
//pb_lectura.height		= 133
//
//gb_2.x 					= This.WorkSpaceWidth() - 351
//gb_2.width				= 275
//
//pb_nuevo.x				= This.WorkSpaceWidth() - 292
//pb_nuevo.width			= 156
//pb_nuevo.height		= 133
//
//pb_insertar.x			= This.WorkSpaceWidth() - 292
//pb_insertar.width		= 156
//pb_insertar.height	= 133
//
//pb_eliminar.x			= This.WorkSpaceWidth() - 292
//pb_eliminar.width		= 156
//pb_eliminar.height	= 133
//
//pb_grabar.x				= This.WorkSpaceWidth() - 292
//pb_grabar.width		= 156
//pb_grabar.height		= 133
//
//pb_imprimir.x			= This.WorkSpaceWidth() - 292
//pb_imprimir.width		= 156
//pb_imprimir.height	= 133
//
//IF st_encabe.Visible THEN
//	gb_2.y 					= dw_1.y - 36
//ELSE
//	gb_2.y 					= gb_1.y + gb_1.height + 23
//END IF
//
//li_posi_y	= gb_2.y - 92
//
//IF pb_nuevo.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_nuevo.y	= li_posi_y
//END IF
//
//IF pb_insertar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_insertar.y	= li_posi_y
//END IF
//
//IF pb_eliminar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_eliminar.y	= li_posi_y
//END IF
//
//IF pb_grabar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_grabar.y		= li_posi_y
//END IF
//
//IF pb_imprimir.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_imprimir.y	= li_posi_y
//END IF
//
//gb_2.height				= 180 * li_objeto + 97
//gb_3.x 					= This.WorkSpaceWidth() - 351
//gb_3.y 					= This.WorkSpaceHeight() - 345
//gb_3.width				= 275
//gb_3.height				= 180 * 1 + 97 /*  (1 Botón)  */
//
//pb_salir.x				= This.WorkSpaceWidth() - 292
//pb_salir.y				= gb_3.y + 88
//pb_salir.width			= 156
//pb_salir.height		= 133
end event

event closequery;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			Message.ReturnValue	=	1 
			
		CASE 0
			CHOOSE CASE 1
//				CASE 1
//					Message.DoubleParm = 0
//					This.triggerevent("ue_guardar")
//					IF message.doubleparm = -1 THEN Message.ReturnValue = 1
//					RETURN
					
				CASE 3
					Message.ReturnValue	=	1
					RETURN
			END CHOOSE
	END CHOOSE
END IF
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_planillasmodificadas
integer x = 64
integer y = 60
integer width = 2523
integer height = 96
string text = "Mantención Planillas Modificadas"
alignment alignment = center!
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_planillasmodificadas
integer x = 2679
integer y = 208
integer taborder = 60
end type

event pb_nuevo::clicked;pb_grabar.Enabled		= False

dw_1.Reset()
dw_1.InsertRow(0)

dw_1.Object.plde_codigo[1]	=	gi_codPlanta
dw_1.Object.espe_codigo[1]	=	ii_especie
dw_1.Object.clie_codigo[1]	=	gi_CodExport


end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_planillasmodificadas
boolean visible = false
integer x = 3109
integer y = 228
integer taborder = 50
end type

event pb_lectura::clicked;call super::clicked;//pb_lectura.Enabled = FALSE
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_planillasmodificadas
boolean visible = false
integer x = 3113
integer y = 724
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_planillasmodificadas
boolean visible = false
integer x = 3118
integer y = 480
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_planillasmodificadas
integer x = 2683
integer y = 704
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_planillasmodificadas
boolean visible = false
integer x = 2738
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_planillasmodificadas
integer x = 2679
integer y = 456
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_planillasmodificadas
integer x = 46
integer y = 252
integer width = 2533
integer height = 704
integer taborder = 70
string dataobject = "dw_mant_planillasmodificadas"
boolean vscrollbar = false
boolean livescroll = false
end type

event dw_1::itemchanged;Long		ll_null 

SetNull(ll_null)

CHOOSE CASE dwo.Name
	
	CASE "clie_codigo"
		IF NOT iuo_cliente.existe(Integer(data), TRUE, SQLCA) THEN
			This.SetItem(row,"clie_codigo", Integer(ll_null))
			Return -1
		ELSE
			ii_cliente = Integer(data)
		END IF
		
	CASE "plde_codigo"
		IF NOT iuo_planta.existe(Integer(data), TRUE, SQLCA) THEN
			This.SetItem(row,"plde_codigo", Integer(ll_null))
			Return -1
		ELSE
			ii_planta = Integer(data)
		END IF
	
	CASE "ccpv_numero","ccpe_numero"
		il_planilla	=	Long(data)
		
		IF ii_especie <> 11 THEN
			IF NOT existeplanillaespecie(il_planilla) THEN
				Parent.TriggerEvent("ue_recuperadatos")
			ELSE
				This.SetItem(1,"ccpv_numero", Long(ll_null))
				Return 1
			END IF
		ELSE
			IF NOT buscaplanillauva(il_planilla) THEN
				Parent.TriggerEvent("ue_recuperadatos")
				
			ELSE
				dw_1.SetItem(1,"ccpe_numero", Long(ll_null))
				
				Return 1
			END IF
		END IF
		
			
END CHOOSE

end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;//CHOOSE CASE dwo.Name
//
//	CASE "ccin_abrevi"
//			pb_grabar.Enabled	=	True
//
////	CASE "cctc_codigo"
////			TriggerEvent("ue_validaregistro")
//
//END CHOOSE
//
end event

event dw_1::clicked;call super::clicked;String	ls_columna

ls_columna = dwo.name

IF  ii_especie = 11 THEN
	CHOOSE CASE ls_columna
		CASE "buscalote"
			buscaloteuva()
		
	END CHOOSE
ELSE
	CHOOSE CASE ls_columna
		CASE "buscalote"
			BuscaLoteespe()
		
	END CHOOSE
END IF	
end event

