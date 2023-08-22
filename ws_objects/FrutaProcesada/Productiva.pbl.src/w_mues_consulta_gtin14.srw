$PBExportHeader$w_mues_consulta_gtin14.srw
forward
global type w_mues_consulta_gtin14 from w_para_informes
end type
type dw_1 from uo_dw within w_mues_consulta_gtin14
end type
type st_1 from statictext within w_mues_consulta_gtin14
end type
type sle_codigo from singlelineedit within w_mues_consulta_gtin14
end type
end forward

global type w_mues_consulta_gtin14 from w_para_informes
integer x = 14
integer y = 32
integer width = 4151
integer height = 2704
string title = "Consulta Códigos de Barra GS1"
string icon = "\Desarrollo\Productiva\FrutaGranelPackingTerceros\ProdFGranelPP.ico"
dw_1 dw_1
st_1 st_1
sle_codigo sle_codigo
end type
global w_mues_consulta_gtin14 w_mues_consulta_gtin14

type variables

end variables

on w_mues_consulta_gtin14.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.st_1=create st_1
this.sle_codigo=create sle_codigo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.sle_codigo
end on

on w_mues_consulta_gtin14.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.st_1)
destroy(this.sle_codigo)
end on

event open;call super::open;dw_1.SetTransObject(Sqlca)
end event

type pb_excel from w_para_informes`pb_excel within w_mues_consulta_gtin14
integer x = 2853
integer y = 736
end type

type st_computador from w_para_informes`st_computador within w_mues_consulta_gtin14
boolean visible = false
end type

type st_usuario from w_para_informes`st_usuario within w_mues_consulta_gtin14
boolean visible = false
end type

type st_temporada from w_para_informes`st_temporada within w_mues_consulta_gtin14
boolean visible = false
end type

type p_logo from w_para_informes`p_logo within w_mues_consulta_gtin14
boolean visible = false
end type

type st_titulo from w_para_informes`st_titulo within w_mues_consulta_gtin14
integer width = 3227
string text = "Consulta Codigos Barra GS1"
end type

type pb_acepta from w_para_informes`pb_acepta within w_mues_consulta_gtin14
string tag = "Imprimir Reporte"
boolean visible = false
integer x = 3703
integer y = 740
integer taborder = 100
string picturename = ""
string disabledname = ""
end type

type pb_salir from w_para_informes`pb_salir within w_mues_consulta_gtin14
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3680
integer y = 1036
integer taborder = 110
string disabledname = "\desarrollo\bmp\exitd.bmp"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_1 from uo_dw within w_mues_consulta_gtin14
integer x = 265
integer y = 684
integer width = 3063
integer height = 1544
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string title = "Codigos de Barra"
string dataobject = "dw_mues_gtin13"
boolean vscrollbar = false
end type

type st_1 from statictext within w_mues_consulta_gtin14
integer x = 306
integer y = 472
integer width = 457
integer height = 136
boolean bringtotop = true
integer textsize = -20
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Codigo"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type sle_codigo from singlelineedit within w_mues_consulta_gtin14
integer x = 782
integer y = 460
integer width = 2674
integer height = 160
integer taborder = 110
integer textsize = -20
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
integer limit = 38
borderstyle borderstyle = stylelowered!
end type

event modified;Double	ld_Numero

If Len(This.Text) < 38 Then
	MessageBox('Atención', 'El largo del Código De Barras debe ser de 38 dígitos.')
	dw_1.Reset()
	Return -1
End If

ld_Numero  =	Double(Mid(This.Text, 3, 14))

This.SetFocus()
This.SelectText(1, Len(This.Text))

If dw_1.Retrieve(ld_Numero) = 0 Then dw_1.Reset()
end event

