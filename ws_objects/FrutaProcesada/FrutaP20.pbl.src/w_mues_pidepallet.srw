$PBExportHeader$w_mues_pidepallet.srw
forward
global type w_mues_pidepallet from w_mant_detalle
end type
type sle_paen_numero from singlelineedit within w_mues_pidepallet
end type
type st_3 from statictext within w_mues_pidepallet
end type
type st_2 from statictext within w_mues_pidepallet
end type
type dw_planta from uo_dw within w_mues_pidepallet
end type
type dw_cliente from uo_dw within w_mues_pidepallet
end type
type st_1 from statictext within w_mues_pidepallet
end type
end forward

global type w_mues_pidepallet from w_mant_detalle
integer height = 792
sle_paen_numero sle_paen_numero
st_3 st_3
st_2 st_2
dw_planta dw_planta
dw_cliente dw_cliente
st_1 st_1
end type
global w_mues_pidepallet w_mues_pidepallet

type variables
DataWindowChild idwc_cliente, idwc_planta
end variables

on w_mues_pidepallet.create
int iCurrent
call super::create
this.sle_paen_numero=create sle_paen_numero
this.st_3=create st_3
this.st_2=create st_2
this.dw_planta=create dw_planta
this.dw_cliente=create dw_cliente
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_paen_numero
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_1
end on

on w_mues_pidepallet.destroy
call super::destroy
destroy(this.sle_paen_numero)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.dw_planta)
destroy(this.dw_cliente)
destroy(this.st_1)
end on

event open;call super::open;
istr_mant = Message.PowerObjectParm

dw_cliente.GetChild('clie_codigo', idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()
dw_cliente.SetTransObject(sqlca)
dw_cliente.InsertRow(0)
dw_cliente.object.clie_codigo[1] = integer(istr_mant.argumento[1])

dw_planta.GetChild('plde_codigo',idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()
dw_planta.SetTransObject(sqlca)
dw_planta.InsertRow(0)
dw_planta.object.plde_codigo[1] = integer(istr_mant.argumento[2])

dw_cliente.Object.clie_codigo.Protect = 1
dw_planta.Object.plde_codigo.Protect = 1
dw_cliente.Object.clie_codigo.BackGround.Color = RGB(166,180,210)
dw_planta.Object.plde_codigo.BackGround.Color = RGB(166,180,210)

istr_mant.Agrega = false
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mues_pidepallet
boolean visible = false
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mues_pidepallet
boolean visible = false
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mues_pidepallet
boolean visible = false
end type

type pb_primero from w_mant_detalle`pb_primero within w_mues_pidepallet
boolean visible = false
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mues_pidepallet
boolean cancel = true
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mues_pidepallet
end type

type pb_salir from w_mant_detalle`pb_salir within w_mues_pidepallet
boolean visible = false
integer taborder = 0
end type

type dw_1 from w_mant_detalle`dw_1 within w_mues_pidepallet
integer y = 112
integer height = 432
integer taborder = 0
string dataobject = "dw_info_vacia"
borderstyle borderstyle = styleraised!
end type

type sle_paen_numero from singlelineedit within w_mues_pidepallet
integer x = 713
integer y = 384
integer width = 402
integer height = 80
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean border = false
integer limit = 8
end type

event modified;IF IsNumber(THIS.text) THEN
	istr_mant.argumento[3] 	=	THIS.text
ELSE
	IF THIS.text <> '' THEN
		THIS.text = ''
		This.SetFocus()
	END IF
END IF
end event

type st_3 from statictext within w_mues_pidepallet
integer x = 197
integer y = 392
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Nro Pallet"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mues_pidepallet
integer x = 197
integer y = 292
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from uo_dw within w_mues_pidepallet
integer x = 704
integer y = 284
integer width = 1175
integer height = 84
integer taborder = 0
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_plantas"
boolean vscrollbar = false
boolean border = false
end type

type dw_cliente from uo_dw within w_mues_pidepallet
integer x = 709
integer y = 188
integer width = 1175
integer height = 84
integer taborder = 0
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_clientesprod"
boolean vscrollbar = false
boolean border = false
end type

type st_1 from statictext within w_mues_pidepallet
integer x = 197
integer y = 196
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

