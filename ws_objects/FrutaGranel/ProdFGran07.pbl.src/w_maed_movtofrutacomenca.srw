$PBExportHeader$w_maed_movtofrutacomenca.srw
forward
global type w_maed_movtofrutacomenca from w_mant_encab_deta
end type
type dw_3 from uo_dw within w_maed_movtofrutacomenca
end type
type dw_4 from uo_dw within w_maed_movtofrutacomenca
end type
end forward

global type w_maed_movtofrutacomenca from w_mant_encab_deta
dw_3 dw_3
dw_4 dw_4
end type
global w_maed_movtofrutacomenca w_maed_movtofrutacomenca

on w_maed_movtofrutacomenca.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
end on

on w_maed_movtofrutacomenca.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_3)
destroy(this.dw_4)
end on

type dw_1 from w_mant_encab_deta`dw_1 within w_maed_movtofrutacomenca
boolean visible = false
integer x = 37
integer y = 1444
integer width = 2889
integer height = 316
string title = "Detalle del movimiento"
string dataobject = "dw_mues_movtofrutacomdeta"
boolean hscrollbar = false
boolean vscrollbar = false
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta`dw_2 within w_maed_movtofrutacomenca
integer x = 37
integer y = 28
integer width = 2889
integer height = 464
string dataobject = "dw_mant_movtofrutacomenca_proceso"
end type

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_maed_movtofrutacomenca
integer x = 3049
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_maed_movtofrutacomenca
integer x = 3049
end type

type pb_grabar from w_mant_encab_deta`pb_grabar within w_maed_movtofrutacomenca
integer x = 3049
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_maed_movtofrutacomenca
integer x = 3049
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_maed_movtofrutacomenca
integer x = 3049
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_maed_movtofrutacomenca
integer x = 3049
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_maed_movtofrutacomenca
integer x = 3049
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_maed_movtofrutacomenca
integer x = 3049
end type

type dw_3 from uo_dw within w_maed_movtofrutacomenca
integer x = 37
integer y = 476
integer width = 2889
integer height = 280
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mant_lotesfrutacomenc_proceso"
boolean vscrollbar = false
borderstyle borderstyle = stylelowered!
end type

type dw_4 from uo_dw within w_maed_movtofrutacomenca
integer x = 37
integer y = 768
integer width = 2889
integer height = 780
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_mues_lotesfrutacomdeta_proceso_bp"
boolean hscrollbar = true
end type

