$PBExportHeader$w_mant_parampredioscuarteles.srw
forward
global type w_mant_parampredioscuarteles from w_mant_tabla
end type
end forward

global type w_mant_parampredioscuarteles from w_mant_tabla
integer width = 2697
integer height = 1340
boolean maxbox = false
boolean resizable = false
event ue_validapassword ( )
end type
global w_mant_parampredioscuarteles w_mant_parampredioscuarteles

type variables
String	is_rut
end variables

event ue_validapassword();istr_mant.Argumento[1]	=	"Predios y Cuarteles"
istr_mant.Argumento[2]	=	gstr_parametros.clave

IF isnull(istr_mant.Argumento[2]) THEN
	istr_mant.Argumento[2] = ''
END IF	

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

event open;Long ll_fila

x				= 0
y				= 0
This.Width	= dw_1.width + 540
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")

istr_mant.dw	= dw_1

buscar	= ""
ordenar	= ""

ll_fila = dw_1.Retrieve()

IF ll_fila = 0 THEN
	dw_1.InsertRow(0)
	dw_1.Object.papc_identi[1] = 1
END IF	

IF ll_fila > 0 THEN PostEvent("ue_validapassword")

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

end event

on w_mant_parampredioscuarteles.create
call super::create
end on

on w_mant_parampredioscuarteles.destroy
call super::destroy
end on

type dw_1 from w_mant_tabla`dw_1 within w_mant_parampredioscuarteles
integer x = 78
integer y = 36
integer width = 1975
integer height = 1128
integer taborder = 20
string dataobject = "dw_mant_parampredioscuarteles"
boolean vscrollbar = false
boolean livescroll = false
end type

event dw_1::clicked;//
end event

event dw_1::rowfocuschanged;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_parampredioscuarteles
boolean visible = false
integer x = 0
integer y = 176
integer width = 2359
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_parampredioscuarteles
boolean visible = false
integer x = 3643
integer y = 128
integer taborder = 0
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_parampredioscuarteles
boolean visible = false
integer x = 3639
integer y = 424
integer taborder = 0
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_parampredioscuarteles
boolean visible = false
integer x = 3639
integer y = 604
integer taborder = 0
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_parampredioscuarteles
boolean visible = false
integer x = 3639
integer y = 784
integer taborder = 0
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_parampredioscuarteles
integer x = 2199
integer y = 248
integer taborder = 30
boolean enabled = true
long backcolor = 553648127
end type

event pb_grabar::clicked;IF dw_1.Update() = 1 THEN
	Commit;
	IF SQLCA.SQLCode <> 0 THEN
		RollBack;
		MessageBox("Grabación de Parámetros", "El Proceso no se pudo ejecutar.~r~n"+&
						"Codigo de Error : "+String(SQLCA.SQLdbCode)+"~r~n"+&
						"Mensaje         : "+SQLCA.SQLErrText)
	ELSE
		MessageBox("Atención","Parámetros de Predios/Cuarteles Grabados.", Exclamation!,Ok!)
		Parametros()		
	END IF
ELSE
	MessageBox("Grabación de Parámetros", "El Proceso no se pudo ejecutar.~r~n"+&
						"Codigo de Error : "+String(SQLCA.SQLdbCode)+"~r~n"+&
						"Mensaje         : "+SQLCA.SQLErrText)
	dw_1.Retrieve()
END IF	
end event

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_parampredioscuarteles
boolean visible = false
integer x = 3639
integer y = 1144
integer taborder = 0
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_parampredioscuarteles
integer x = 2199
integer y = 540
integer taborder = 40
long backcolor = 553648127
end type

