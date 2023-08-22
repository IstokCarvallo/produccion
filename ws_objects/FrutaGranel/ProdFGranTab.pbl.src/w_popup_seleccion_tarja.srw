$PBExportHeader$w_popup_seleccion_tarja.srw
forward
global type w_popup_seleccion_tarja from window
end type
type pb_1 from picturebutton within w_popup_seleccion_tarja
end type
type pb_aceptar from picturebutton within w_popup_seleccion_tarja
end type
type dw_1 from datawindow within w_popup_seleccion_tarja
end type
end forward

global type w_popup_seleccion_tarja from window
integer width = 1559
integer height = 816
boolean titlebar = true
string title = "SELECCION DE LOTE A VACIAR"
windowtype windowtype = response!
long backcolor = 16777215
string icon = "AppIcon!"
boolean center = true
pb_1 pb_1
pb_aceptar pb_aceptar
dw_1 dw_1
end type
global w_popup_seleccion_tarja w_popup_seleccion_tarja

type variables
str_mant		istr_mant

Integer		ii_cliente
Long			il_planta, il_tarja, il_filas
end variables

event open;Integer	li_filas

istr_mant	=	Message.PowerObjectParm	

ii_cliente	=	Integer(istr_mant.Argumento[1])
il_planta	=	Long(istr_mant.Argumento[2])
il_tarja	=	Long(istr_mant.Argumento[3])

dw_1.SetTransObject(sqlca)
li_filas	=	dw_1.Retrieve(ii_cliente, il_planta, il_tarja)

IF li_filas < 1 THEN
	Close(This)
ELSE
	dw_1.SetRowFocusIndicator(Hand!)
	dw_1.SetRow(1)
	dw_1.SelectRow(1, True)
	il_filas	=	1
	
	IF il_filas > 0 THEN
		istr_mant.Argumento[4]	=	String(dw_1.Object.lote_pltcod[il_filas])
		istr_mant.Argumento[5]	=	String(dw_1.Object.lote_espcod[il_filas])
		istr_mant.Argumento[6]	=	String(dw_1.Object.lote_codigo[il_filas])
	END IF
END IF
end event

on w_popup_seleccion_tarja.create
this.pb_1=create pb_1
this.pb_aceptar=create pb_aceptar
this.dw_1=create dw_1
this.Control[]={this.pb_1,&
this.pb_aceptar,&
this.dw_1}
end on

on w_popup_seleccion_tarja.destroy
destroy(this.pb_1)
destroy(this.pb_aceptar)
destroy(this.dw_1)
end on

type pb_1 from picturebutton within w_popup_seleccion_tarja
integer x = 1202
integer y = 472
integer width = 302
integer height = 244
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
alignment htextalign = left!
end type

event clicked;
CloseWithReturn(Parent, istr_mant)
end event

type pb_aceptar from picturebutton within w_popup_seleccion_tarja
integer x = 1202
integer y = 136
integer width = 300
integer height = 245
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
alignment htextalign = left!
end type

event clicked;CloseWithReturn(Parent, istr_mant)
end event

type dw_1 from datawindow within w_popup_seleccion_tarja
integer x = 37
integer y = 20
integer width = 1065
integer height = 696
integer taborder = 10
boolean titlebar = true
string title = "Tarjas Disponibles"
string dataobject = "dw_selectividad_tarjas_lotes"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;il_filas	=	row

dw_1.SelectRow(0, False)
dw_1.SelectRow(il_filas, True)
dw_1.SetRow(il_filas)
dw_1.ScrollToRow(il_filas)

IF il_filas > 0 THEN
	istr_mant.Argumento[4]	=	String(dw_1.Object.lote_pltcod[il_filas])
	istr_mant.Argumento[5]	=	String(dw_1.Object.lote_espcod[il_filas])
	istr_mant.Argumento[6]	=	String(dw_1.Object.lote_codigo[il_filas])
END IF
end event

event doubleclicked;il_filas	=	row

dw_1.SelectRow(0, False)
dw_1.SelectRow(il_filas, True)
dw_1.SetRow(il_filas)
dw_1.ScrollToRow(il_filas)

IF il_filas > 0 THEN
	istr_mant.Argumento[4]	=	String(dw_1.Object.lote_pltcod[il_filas])
	istr_mant.Argumento[5]	=	String(dw_1.Object.lote_espcod[il_filas])
	istr_mant.Argumento[6]	=	String(dw_1.Object.lote_codigo[il_filas])
END IF

CloseWithReturn(Parent, istr_mant)
end event

