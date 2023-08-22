$PBExportHeader$w_mant_deta_recfruprocee.srw
forward
global type w_mant_deta_recfruprocee from w_mant_detalle_csd
end type
type dw_2 from uo_dw within w_mant_deta_recfruprocee
end type
end forward

global type w_mant_deta_recfruprocee from w_mant_detalle_csd
integer width = 3058
integer height = 1864
string icon = ""
dw_2 dw_2
end type
global w_mant_deta_recfruprocee w_mant_deta_recfruprocee

type variables
Date		id_FechaAcceso
Time		it_HoraAcceso


end variables

on w_mant_deta_recfruprocee.create
int iCurrent
call super::create
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
end on

on w_mant_deta_recfruprocee.destroy
call super::destroy
destroy(this.dw_2)
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.GetItemNumber(il_fila, "plde_codigo"))
ias_campo[2] = String(dw_1.GetItemNumber(il_fila, "clie_codigo"))
ias_campo[3] = String(dw_1.GetItemNumber(il_fila, "rfpe_numero"))
ias_campo[4] = String(dw_1.GetItemNumber(il_fila, "paen_numero"))
ias_campo[5] = String(dw_1.GetItemNumber(il_fila, "paen_tipopa"))
ias_campo[6] = dw_1.GetItemString(il_fila, "vari_nombre")
ias_campo[7] = dw_1.GetItemString(il_fila, "emba_codigo")
ias_campo[8] = String(dw_1.GetItemNumber(il_fila, "cate_codigo"))
ias_campo[9] = String(dw_1.GetItemNumber(il_fila, "stat_codigo"))





end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.rfpe_numero[il_fila]) OR dw_1.Object.rfpe_numero[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNúmero de Recepción"
	ls_colu[li_cont]	= "rfpe_numero"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_guardar;call super::ue_guardar;IF Message.DoubleParm = -1 THEN RETURN

dw_1.Object.inpr_numero[il_fila]	= Long(istr_mant.argumento[2])
end event

event ue_deshace;call super::ue_deshace;dw_1.SetItem(il_fila, "plde_codigo",Integer(ias_campo[1]))
dw_1.SetItem(il_fila, "clie_codigo",Integer(ias_campo[2])) 
dw_1.SetItem(il_fila, "rfpe_numero",Integer(ias_campo[3])) 
dw_1.SetItem(il_fila, "paen_numero",Long(ias_campo[4])) 
dw_1.SetItem(il_fila, "paen_tipopa",Integer(ias_campo[5])) 
dw_1.SetItem(il_fila, "vari_nombre",ias_campo[6]) 
dw_1.SetItem(il_fila, "emba_codigo",ias_campo[7]) 
dw_1.SetItem(il_fila, "cate_codigo",Integer(ias_campo[8]))
dw_1.SetItem(il_fila, "stat_codigo",Integer(ias_campo[9]))
end event

event open;call super::open;GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_recfruprocee
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_recfruprocee
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_recfruprocee
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_recfruprocee
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_recfruprocee
integer x = 2711
integer y = 368
integer taborder = 40
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_recfruprocee
integer x = 2711
integer y = 152
integer taborder = 30
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_recfruprocee
integer x = 2711
integer y = 584
integer taborder = 50
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_recfruprocee
integer x = 160
integer y = 124
integer width = 2469
integer height = 860
integer taborder = 20
string dataobject = "dw_mant_recfruprocee_detalle"
end type

type dw_2 from uo_dw within w_mant_deta_recfruprocee
boolean visible = false
integer x = 160
integer y = 972
integer width = 2478
integer height = 704
integer taborder = 10
boolean enabled = false
string dataobject = "dw_mant_recfruprocee_inforproduc"
boolean vscrollbar = false
boolean border = false
end type

