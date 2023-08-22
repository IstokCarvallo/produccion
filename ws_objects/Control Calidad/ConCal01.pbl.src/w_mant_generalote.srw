$PBExportHeader$w_mant_generalote.srw
forward
global type w_mant_generalote from w_mant_tabla
end type
end forward

global type w_mant_generalote from w_mant_tabla
integer width = 2414
integer height = 1052
string title = "PARÁMETROS"
event ue_validapassword ( )
end type
global w_mant_generalote w_mant_generalote

type variables
String	is_rut

end variables

forward prototypes
public subroutine parametros ()
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Control de Calidad"
istr_mant.Argumento[2]	=	gstr_parlote.paswor

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

public subroutine parametros ();gstr_parlote.identi = dw_1.Object.pacc_identi[1]
gstr_parlote.codgen = dw_1.Object.pacc_codgen[1]
gstr_parlote.desgen = dw_1.Object.pacc_desgen[1]
gstr_parlote.paswor  = dw_1.Object.pacc_paswor[1]

end subroutine

event open;x				= 0
y				= 0

im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")

If dw_1.Retrieve() = 0 Then dw_1.InsertRow(0)

PostEvent("ue_validapassword")

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

end event

on w_mant_generalote.create
call super::create
end on

on w_mant_generalote.destroy
call super::destroy
end on

event resize;//
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_generalote
integer x = 155
integer y = 84
integer width = 1838
integer height = 800
integer taborder = 20
string dataobject = "dw_mant_generalote"
boolean vscrollbar = false
boolean livescroll = false
end type

event dw_1::clicked;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::getfocus;//
end event

event dw_1::doubleclicked;//
end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	IF dwo.Name = "empr_rutemp" THEN
		IF is_rut <> "" THEN
			This.SetItem(1, "empr_rutemp", String(Double(Mid(is_rut, 1, 9)), "#########") + Mid(is_rut, 10))
		END IF
	ELSE
		This.SetItem(1, "empr_rutemp", is_rut)
	END IF
END IF
end event

event dw_1::itemchanged;call super::itemchanged;String	ls_Null

SetNull(ls_Null)

CHOOSE CASE dwo.Name
			
	CASE "empr_rutemp"
		is_rut = F_verrut(data, True)
		
		IF is_rut = ""  THEN
			This.SetItem(1, "empr_rutemp", ls_Null)
			RETURN 1
		END IF

END CHOOSE


end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_generalote
boolean visible = false
integer x = 160
integer y = 108
integer width = 1829
integer height = 104
alignment alignment = center!
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_generalote
boolean visible = false
integer x = 3643
integer y = 128
integer taborder = 0
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_generalote
boolean visible = false
integer x = 3639
integer taborder = 0
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_generalote
boolean visible = false
integer x = 3639
integer y = 604
integer taborder = 0
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_generalote
boolean visible = false
integer x = 3639
integer y = 784
integer taborder = 0
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_generalote
integer x = 2107
integer y = 248
integer taborder = 30
boolean enabled = true
end type

event pb_grabar::clicked;IF Isnull(dw_1.object.pacc_paswor[1]) OR dw_1.object.pacc_paswor[1]='' THEN
	messagebox("Atención","Password no puede ser Nula",exclamation!)
	dw_1.setfocus()
	return
ELSEIF dw_1.Update() = 1 THEN
	Commit;
	IF SQLCA.SQLCode <> 0 THEN
		RollBack;
		MessageBox("Grabación de Parámetros", "El Proceso no se pudo ejecutar.~r~n"+&
						"Codigo de Error : "+String(SQLCA.SQLdbCode)+"~r~n"+&
						"Mensaje         : "+SQLCA.SQLErrText)
	ELSE
		MessageBox("Atención","Parámetros de Sistema Grabados.", Exclamation!,Ok!)
		parametros()
	END IF
ELSE
	MessageBox("Grabación de Parámetros", "El Proceso no se pudo ejecutar.~r~n"+&
						"Codigo de Error : "+String(SQLCA.SQLdbCode)+"~r~n"+&
						"Mensaje         : "+SQLCA.SQLErrText)
	dw_1.Retrieve()
END IF	
end event

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_generalote
boolean visible = false
integer x = 3639
integer y = 1144
integer taborder = 0
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_generalote
integer x = 2117
integer y = 452
integer taborder = 40
end type

