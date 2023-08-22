$PBExportHeader$w_emis_guia_despacho_envases.srw
$PBExportComments$Determina correlativo para Guía de Despacho de Envases
forward
global type w_emis_guia_despacho_envases from w_mant_detalle
end type
type dw_2 from datawindow within w_emis_guia_despacho_envases
end type
type str_analisis from structure within w_emis_guia_despacho_envases
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

global type w_emis_guia_despacho_envases from w_mant_detalle
integer width = 3689
integer height = 1100
string title = "Emisión Guía SII Envases"
dw_2 dw_2
end type
global w_emis_guia_despacho_envases w_emis_guia_despacho_envases

type variables
Decimal {2}	id_valpar
str_info		istr_info

Long			il_NumeroGuia
Integer		ii_Planta
Boolean		ib_imprimio = False, ib_anula = False
DataWindowChild	idwc_timbredespacho

uo_tipomovtofruta	iuo_TipoMovto
uo_usuarplanta		iuo_UsuaPlta
uo_ubicacion		iuo_Ubicac
uo_plantadesp		iuo_Planta
uo_ClientesProd	iuo_Cliente
uo_GuiaDespacho	iuo_Guia
end variables

forward prototypes
public function long numeroguia ()
end prototypes

public function long numeroguia ();Long		ll_NumeroGuia

SELECT	IsNull(corr_ultdoc, 0) + 1
	INTO	:ll_NumeroGuia
	FROM	dbo.spro_correldoctos
	WHERE	plde_codigo	=	:ii_Planta
	AND	ubdo_codigo	=	:iuo_UsuaPlta.Ubicacion
	AND	tdop_codigo	=	2 ;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Correlativo de Documentos")
	
	SetNull(ll_NumeroGuia)
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No ha sido definido Correlativo para Guías de " + &
					"Despacho en :~r~tPlanta " + iuo_Planta.Nombre + "~r~t" + &
					"Ubicación " + iuo_Ubicac.Nombre + &
					"~r~rNo será posible la Emisión del Documento.")
	
	SetNull(ll_NumeroGuia)
END IF

RETURN ll_NumeroGuia
end function

on w_emis_guia_despacho_envases.create
int iCurrent
call super::create
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
end on

on w_emis_guia_despacho_envases.destroy
call super::destroy
destroy(this.dw_2)
end on

event ue_recuperadatos;Long		ll_Guia

w_main.SetMicroHelp("Recuperando Datos...")

SetPointer(HourGlass!)
PostEvent("ue_listo")

il_fila	=	dw_1.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]), Integer(istr_mant.argumento[5]))
If il_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	Close(This)
ElseIf il_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If gi_Emisor_Electronico = 1 Then //And iuo_Cliente.Guia_Electronica = 1 Then GHerrera, para emision de GDE Comercial para todos los clientes.
		If dw_1.Object.meen_guiemi[1] = 0 Then 
			ll_Guia = iuo_Guia.of_EmiteGuia(dw_1.Object.plde_codigo[1], dw_1.Object.clie_codigo[1], 2)
			If ll_Guia > 0 Then
				
				dw_1.Object.meen_guisii.Protect	=	1
				dw_1.Object.meen_guisii.Color		=	Rgb(255,255,255)
				dw_1.Object.meen_guisii.BackGround.Color	=	553648127

				dw_1.Object.meen_despac.Protect	=	1
				dw_1.Object.meen_despac.Color		=	Rgb(255,255,255)
				dw_1.Object.meen_despac.BackGround.Color	=	553648127
				
				dw_1.Object.meen_guisii[1]		=	ll_Guia
				dw_1.Object.meen_despac[1]	=	gstr_Us.Nombre
				dw_1.Object.meen_guiemi[1]	=	3
				pb_salir.Enabled	= False
				TriggerEvent('ue_guardar')
			Else
				MessageBox('Alerta', 'No se pudo obtener numero de guias de despacho.', Exclamation!, OK!)
				Message.DoubleParm = -1
			End If
		Else
			dw_1.Object.meen_guisii.Protect	=	1
			dw_1.Object.meen_guisii.Color		=	Rgb(255,255,255)
			dw_1.Object.meen_guisii.BackGround.Color	=	553648127

			dw_1.Object.meen_despac.Protect	=	1
			dw_1.Object.meen_despac.Color		=	Rgb(255,255,255)
			dw_1.Object.meen_despac.BackGround.Color	=	553648127
			
			pb_salir.Enabled	= False
		End If
	Else
//		dw_1.Object.meen_guisii.Protect	=	1
//		dw_1.Object.meen_guisii.Color		=	Rgb(255,255,255)
//		dw_1.Object.meen_guisii.BackGround.Color	=	553648127
//
//		dw_1.Object.meen_despac.Protect	=	1
//		dw_1.Object.meen_despac.Color		=	Rgb(255,255,255)
//		dw_1.Object.meen_despac.BackGround.Color	=	553648127
		
		dw_1.Object.meen_despac[1]	=	gstr_Us.Nombre	
	End If	
End If
end event

event open;/* 
	Argumentos
		istr_Mant.Argumento[1]	=	Código Planta
		istr_Mant.Argumento[2]	=	Tipo de Movimiento
		istr_Mant.Argumento[3]	=	Número de Despacho
		istr_Mant.Argumento[4]	=	Sentido del Movimiento => 2 = Despacho
		istr_Mant.Argumento[5]	=	Cliente
*/
x	= 100
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

dw_1.GetChild("refg_timbre", idwc_timbredespacho)
idwc_timbredespacho.SetTransObject(sqlca)

If idwc_timbredespacho.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Timbres de Despacho")
	idwc_timbredespacho.InsertRow(0)
End If

dw_1.SetTransObject(sqlca)

iuo_TipoMovto.Existe(Integer(istr_Mant.Argumento[2]), False, sqlca)
iuo_UsuaPlta.Existe(gstr_us.Nombre, ii_Planta, False, sqlca)
iuo_Ubicac.Existe(iuo_UsuaPlta.Ubicacion, False, sqlca)
iuo_Planta.Existe(ii_Planta, False, sqlca)
iuo_Cliente.Existe(Integer(istr_Mant.Argumento[5]), False, sqlca)

dw_1.GetChild("refg_timbre", idwc_timbredespacho)
idwc_timbredespacho.SetTransObject(sqlca)

If idwc_timbredespacho.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Timbres de Despacho")
	idwc_timbredespacho.InsertRow(0)
End If

//dw_2.DataObject	=	"dw_info_guia_despacho_envase"

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
	IF sqlca.sqlcode = -1 THEN
		w_main.SetMicroHelp("No se puede Grabar información.")
		Rollback;
		Message.DoubleParm = -1
		RETURN
	ELSE
		w_main.SetMicroHelp("Información Grabada.")
		Commit;
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

type pb_ultimo from w_mant_detalle`pb_ultimo within w_emis_guia_despacho_envases
string tag = ""
boolean visible = false
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_emis_guia_despacho_envases
string tag = ""
boolean visible = false
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_emis_guia_despacho_envases
string tag = ""
boolean visible = false
end type

type pb_primero from w_mant_detalle`pb_primero within w_emis_guia_despacho_envases
string tag = ""
boolean visible = false
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_emis_guia_despacho_envases
string tag = ""
integer x = 3287
integer y = 384
end type

event pb_cancela::clicked;istr_mant.Respuesta = 0

CloseWithReturn(Parent, istr_mant)
end event

type pb_acepta from w_mant_detalle`pb_acepta within w_emis_guia_despacho_envases
string tag = ""
integer x = 3287
integer y = 144
fontcharset fontcharset = ansi!
string text = " "
string picturename = "\Desarrollo 17\Imagenes\Botones\Imprimir.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Imprimir-bn.png"
end type

event pb_acepta::clicked;Long	ll_NroGuia, li_EmisionGuia, ll_NUmero, ll_TipoMovto, Fila

SetPointer(HourGlass!)

If IsNull(dw_1.Object.refg_timbre[1]) Or dw_1.Object.refg_timbre[1] = 0 Then
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

ll_NroGuia			=	dw_1.Object.meen_guisii[1]
li_EmisionGuia		=	dw_1.Object.meen_guiemi[1]
ll_Numero			=	dw_1.Object.meen_numero[1]
ll_TipoMovto		=	dw_1.Object.tpmv_codigo[1]
	
istr_info.titulo	= "MOVIMIENTO DE ENVASES"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_guia_despacho_cal"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1], ll_Numero, Integer(istr_Mant.Argumento[6]), ll_TipoMovto)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else	
	If gi_Emisor_Electronico = 1 Then //And iuo_Cliente.Guia_Electronica = 1 Then	
		If li_EmisionGuia = 3 Or li_EmisionGuia = 0 Then 
			If iuo_Guia.of_EmiteGuia(dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1], ll_NroGuia, False,ll_Numero, Integer(istr_Mant.Argumento[6]), &
											ll_TipoMovto, 0, 0, 0, 0, Integer(istr_Mant.Argumento[6]), -1)  > 0 Then
				If Not iuo_Guia.of_GeneraLibroGuia(2) Then 
					MessageBox('Alerta', 'No se pudo actualziar Libro de guias de despacho.', Information!, OK!)
				End If
				iuo_Guia.of_ActualizaEstadoGD(1, dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1], ll_NroGuia, 2, SQLCA)
				iuo_Guia.of_RecuperaPDF(ll_NroGuia, dw_1.Object.meen_fecmov[1], 2)
			End If
		Else
			iuo_Guia.of_RecuperaPDF(ll_NroGuia, dw_1.Object.meen_fecmov[1], 2)
		End If
	End If
End If

SetPointer(Arrow!)

CloseWithReturn(Parent, istr_Mant)
end event

type pb_salir from w_mant_detalle`pb_salir within w_emis_guia_despacho_envases
string tag = ""
integer x = 3287
integer y = 652
boolean enabled = false
end type

event pb_salir::clicked;istr_mant.Respuesta = 0

CloseWithReturn(Parent,istr_mant)
end event

type dw_1 from w_mant_detalle`dw_1 within w_emis_guia_despacho_envases
integer y = 132
integer width = 3109
integer height = 772
string dataobject = "dw_mant_movtoenvaenca_emision"
boolean hscrollbar = true
end type

type dw_2 from datawindow within w_emis_guia_despacho_envases
boolean visible = false
integer x = 654
integer width = 155
integer height = 116
integer taborder = 41
boolean bringtotop = true
string dataobject = "dw_info_guia_despacho_cal"
boolean livescroll = true
end type

