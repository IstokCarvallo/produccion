$PBExportHeader$w_emis_guia_despacho.srw
$PBExportComments$Completa información y determina correlativo para Guía de Despacho.
forward
global type w_emis_guia_despacho from w_mant_detalle_csd
end type
type dw_2 from datawindow within w_emis_guia_despacho
end type
type str_analisis from structure within w_emis_guia_despacho
end type
end forward

type str_analisis from structure
	string		rut
	string		nombre
	string		direcc
	string		contac
	integer		ciudad
	string		comuna
end type

global type w_emis_guia_despacho from w_mant_detalle_csd
integer width = 3232
integer height = 1472
dw_2 dw_2
end type
global w_emis_guia_despacho w_emis_guia_despacho

type variables
Decimal {2}	id_valpar
str_info		istr_info

Long			il_NumeroGuia
Integer		ii_Planta
Boolean		ib_imprimio = False, ib_anula = False

uo_tipomovtofruta	iuo_TipoMovto
uo_usuarplanta		iuo_UsuaPlta
uo_ubicacion		iuo_Ubicac
uo_plantadesp		iuo_Planta
uo_ClientesProd	iuo_Cliente
uo_GuiaDespacho	iuo_Guia

Datawindowchild idwc_timbredespacho
end variables

on w_emis_guia_despacho.create
int iCurrent
call super::create
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
end on

on w_emis_guia_despacho.destroy
call super::destroy
destroy(this.dw_2)
end on

event ue_recuperadatos;Long		ll_Guia

w_main.SetMicroHelp("Recuperando Datos...")

SetPointer(HourGlass!)
PostEvent("ue_listo")

il_fila	=	dw_1.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), &
								Long(istr_mant.argumento[3]), Long(istr_mant.argumento[5]))

IF il_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	Close(This)
ELSEIF il_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	
	If gi_Emisor_Electronico = 1 Then //And iuo_Cliente.Guia_Electronica = 1 Then
		If dw_1.Object.mfge_guiemi[1] = 0 Then 
			ll_Guia = iuo_Guia.of_EmiteGuia(dw_1.Object.plde_codigo[1], dw_1.Object.clie_codigo[1], 2)
			If ll_Guia > 0 Then
				dw_1.Object.mfge_guisii.Protect				=	1
				dw_1.Object.mfge_guisii.Color					=	Rgb(255,255,255)
				dw_1.Object.mfge_guisii.BackGround.Color	=	553648127
				
				dw_1.Object.mfge_guisii[1]		=	ll_Guia
				dw_1.Object.mfge_guiemi[1]	=	3
				istr_Mant.Argumento[1]			=	String(ll_Guia)
				
				pb_salir.Enabled	= False
				TriggerEvent('ue_guardar')
			Else
				MessageBox('Alerta', 'No se pudo obtener numero de guias de despacho.', Exclamation!, OK!)
				Message.DoubleParm = -1
			End If
		Else
			dw_1.Object.mfge_guisii.Protect	=	1
			dw_1.Object.mfge_guisii.Color		=	Rgb(255,255,255)
			dw_1.Object.mfge_guisii.BackGround.Color	=	553648127
			
			pb_salir.Enabled	= False
		End If
	End If
END IF
end event

event open;x	= 100
y	= 450

PostEvent("ue_recuperadatos")

istr_Mant = Message.PowerObjectParm

iuo_TipoMovto	=	Create uo_tipomovtofruta
iuo_UsuaPlta	=	Create uo_usuarplanta
iuo_Ubicac		=	Create uo_ubicacion
iuo_Planta		=	Create uo_plantadesp
iuo_Cliente		=	Create uo_ClientesProd
iuo_Guia			=	Create uo_GuiaDespacho

ii_Planta		=	Integer(istr_Mant.Argumento[1])

dw_1.GetChild("tica_codigo", idwc_timbredespacho)
idwc_timbredespacho.SetTransObject(sqlca)

IF idwc_timbredespacho.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Timbres de Despacho")
	idwc_timbredespacho.InsertRow(0)
END IF

dw_1.SetTransObject(sqlca)

iuo_TipoMovto.Existe(Integer(istr_Mant.Argumento[2]), False, sqlca)
iuo_UsuaPlta.Existe(gstr_us.Nombre, ii_Planta, False, sqlca)
iuo_Ubicac.Existe(iuo_UsuaPlta.Ubicacion, False, sqlca)
iuo_Planta.Existe(ii_Planta, False, sqlca)

dw_2.DataObject	=	"dw_info_guia_despacho_cal"

dw_2.SetTransObject(sqlca)
end event

event closequery;//
end event

event ue_guardar;call super::ue_guardar;IF dw_1.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

IF dw_1.Update() = 1 THEN
	Commit;
	IF sqlca.sqlcode = -1 THEN
		w_main.SetMicroHelp("No se puede Grabar información.")
		Rollback;
		Message.DoubleParm = -1
		RETURN
	ELSE
		w_main.SetMicroHelp("Información Grabada.")
		RETURN
	END IF
ELSE
	Rollback;
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_nuevo;//
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_emis_guia_despacho
string tag = ""
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_emis_guia_despacho
string tag = ""
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_emis_guia_despacho
string tag = ""
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_emis_guia_despacho
string tag = ""
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_emis_guia_despacho
string tag = ""
integer x = 2839
integer y = 476
end type

event pb_cancela::clicked;istr_mant.Respuesta = 0

CloseWithReturn(Parent, istr_mant)
end event

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_emis_guia_despacho
string tag = ""
integer x = 2834
integer y = 176
string text = " "
string picturename = "\Desarrollo 17\Imagenes\Botones\Imprimir.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Imprimir-bn.png"
end type

event pb_acepta::clicked;Long	ll_NroGuia, li_EmisionGuia, ll_NUmero, ll_TipoMovto, Fila

SetPointer(HourGlass!)

If IsNull(dw_1.Object.tide_codigo[1]) Or dw_1.Object.tide_codigo[1] = 0 Then
	MessageBox('Atencion', 'Debe seleccionar un Timbre, para la emision de la Guia Despacho Electronica.', Exclamation!, OK!)
	dw_1.SetFocus()
	Return
End If

If dw_1.Update() = 1 Then
	Commit;
	istr_mant.Respuesta = 1
Else
	RollBack;
	istr_mant.Respuesta = 0
End If

ll_NroGuia			=	dw_1.Object.mfge_guisii[1]
li_EmisionGuia		=	dw_1.Object.mfge_guiemi[1]
ll_Numero			=	dw_1.Object.mfge_numero[1]
ll_TipoMovto		=	dw_1.Object.tpmv_codigo[1]
	
istr_info.titulo	= "MOVIMIENTO DE ENVASES"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_guia_despacho_cal"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1], ll_Numero, Integer(istr_Mant.Argumento[6]), ll_TipoMovto)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else	
	If li_EmisionGuia = 3 Or li_EmisionGuia = 0 Then 
		If iuo_Guia.of_EmiteGuia(dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1], ll_NroGuia, False,ll_Numero, Integer(istr_Mant.Argumento[6]), &
										ll_TipoMovto, 0, 0, 0, 0, Integer(istr_Mant.Argumento[6]),-1)  > 0 Then
			If Not iuo_Guia.of_GeneraLibroGuia(2) Then 
				MessageBox('Alerta', 'No se pudo actualziar Libro de guias de despacho.', Information!, OK!)
			End If
			iuo_Guia.of_ActualizaEstadoGD(1, dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1], ll_NroGuia, Integer(istr_Mant.Argumento[6]), SQLCA)
			iuo_Guia.of_RecuperaPDF(ll_NroGuia, dw_1.Object.mfge_fecmov[1], 2)
		End If
	Else
		iuo_Guia.of_RecuperaPDF(ll_NroGuia, dw_1.Object.mfge_fecmov[1], 2)
	End If
End If

SetPointer(Arrow!)

CloseWithReturn(Parent, istr_Mant)
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_emis_guia_despacho
string tag = ""
integer x = 2848
integer y = 784
boolean enabled = false
end type

event pb_salir::clicked;istr_mant.Respuesta = 0

CloseWithReturn(Parent,istr_mant)
end event

type dw_1 from w_mant_detalle_csd`dw_1 within w_emis_guia_despacho
integer x = 59
integer y = 52
integer width = 2601
integer height = 1280
string dataobject = "dw_mant_movtofrutagranel_emision"
end type

event dw_1::itemchanged;call super::itemchanged;Integer	li_null, li_bodega, li_tipdoc

SetNull(li_null)

CHOOSE CASE dwo.Name
	CASE "mden_numero"
//		li_bodega	= This.Object.bode_codigo[row]
//		li_tipdoc	= This.Object.mden_tipdoc[row]
		
//		IF ExisteDocto(Long(data)) OR Not Folio_Valido(li_bodega, li_tipdoc, Long(data)) THEN
//			This.Object.mden_numero[row]	= li_null
//			RETURN 1
//		ELSE
//			istr_mant.argumento[2]	= data
//		END IF
END CHOOSE

end event

type dw_2 from datawindow within w_emis_guia_despacho
boolean visible = false
integer x = 2802
integer y = 1088
integer width = 302
integer height = 228
integer taborder = 41
boolean bringtotop = true
string dataobject = "dw_info_guia_despacho_cal"
boolean vscrollbar = true
boolean livescroll = true
end type

