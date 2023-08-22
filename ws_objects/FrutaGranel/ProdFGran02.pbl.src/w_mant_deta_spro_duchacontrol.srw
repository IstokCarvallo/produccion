$PBExportHeader$w_mant_deta_spro_duchacontrol.srw
forward
global type w_mant_deta_spro_duchacontrol from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_spro_duchacontrol from w_mant_detalle_csd
integer width = 2610
integer height = 1536
string title = "CIERRE CONTROL DUCHA"
end type
global w_mant_deta_spro_duchacontrol w_mant_deta_spro_duchacontrol

type variables

end variables

forward prototypes
public function boolean duplicado (string campo, integer tipo)
end prototypes

public function boolean duplicado (string campo, integer tipo);Long		ll_fila
String	ls_codigo

ls_codigo	= String(dw_1.GetItemNumber(il_fila,"prov_codigo"))

CHOOSE CASE tipo
	CASE 1
		ls_codigo	= campo

END CHOOSE

ll_fila	= dw_1.Find("prov_codigo = " + ls_codigo, &
							1, dw_1.RowCount())

IF ll_fila > 0 AND ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresada anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_spro_duchacontrol.create
call super::create
end on

on w_mant_deta_spro_duchacontrol.destroy
call super::destroy
end on

event ue_recuperadatos();call super::ue_recuperadatos;ias_campo[1] = dw_1.object.duch_codigo[il_fila]
ias_campo[2] = dw_1.object.codu_nropos[il_fila]
ias_campo[3] = String(dw_1.object.codu_fecini[il_fila])
ias_campo[4] = String(dw_1.object.codu_horini[il_fila])
ias_campo[5] = String(dw_1.object.codu_fecter[il_fila])
ias_campo[6] = String(dw_1.object.codu_horter[il_fila])
ias_campo[7] = String(dw_1.object.codu_canbul[il_fila])
ias_campo[8] = String(dw_1.object.codu_observ[il_fila])


IF istr_mant.agrega = False and istr_mant.borra = False THEN
	dw_1.settaborder("duch_codigo",0)
	dw_1.modify("duch_codigo.background.color = " + string(rgb(192,192,192)))
END IF
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "duch_codigo", ias_campo[1])
	dw_1.SetItem(il_fila, "codu_nropos", ias_campo[2])
	dw_1.SetItem(il_fila, "codu_fecini", ias_campo[3])
	dw_1.SetItem(il_fila, "codu_horini", ias_campo[4])
	dw_1.SetItem(il_fila, "codu_fecter", ias_campo[5])
	dw_1.SetItem(il_fila, "codu_horter", ias_campo[6])
	dw_1.SetItem(il_fila, "codu_canbul", ias_campo[7])
	dw_1.SetItem(il_fila, "codu_observ", ias_campo[8])
	
END IF






end event

event ue_antesguardar();Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.GetItemNumber(il_fila, "prov_codigo")) OR dw_1.GetItemNumber(il_fila, "prov_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Provincia"
	ls_colu[li_cont]	= "prov_codigo"
END IF

IF Isnull(dw_1.GetItemString(il_fila, "prov_nombre")) OR dw_1.GetItemString(il_fila, "prov_nombre") = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNombre Provincia"
	ls_colu[li_cont]	= "prov_nombre"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event open;x	= 100
y	= 450

istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

This.TriggerEvent("ue_recuperadatos")



//x	= 100
//y	= 450
//
//This.Icon	=	Gstr_apl.Icono
//
//PostEvent("ue_recuperadatos")
//
//istr_mant = Message.PowerObjectParm
//
//dw_1.SetTransObject(sqlca)
//istr_mant.dw.ShareData(dw_1)
end event

event ue_nuevo();call super::ue_nuevo;//	dw_1.SetItem(il_fila, "regi_codigo", Integer(istr_mant.argumento[1]))
//	dw_1.SetItem(il_fila, "prov_codigo", Integer(istr_mant.argumento[2]))
//   dw_1.SetItem(il_fila, "prov_nombre", istr_mant.argumento[3])
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_spro_duchacontrol
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_spro_duchacontrol
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_spro_duchacontrol
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_spro_duchacontrol
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_spro_duchacontrol
integer x = 2258
integer y = 400
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_spro_duchacontrol
integer x = 2258
integer y = 184
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_spro_duchacontrol
integer x = 2258
integer y = 616
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_spro_duchacontrol
integer x = 87
integer y = 52
integer width = 1970
integer height = 1208
string dataobject = "dw_mant_duchacontrol_cierre"
end type

event dw_1::itemchanged;call super::itemchanged;String  ls_columna

ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "prov_codigo"
		IF Duplicado(data, 1) THEN
			This.SetItem(il_fila, ls_columna, Integer(ias_campo[1]))
			RETURN 1
		END IF
		
END CHOOSE
end event

