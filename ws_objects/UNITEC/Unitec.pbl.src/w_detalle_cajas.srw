$PBExportHeader$w_detalle_cajas.srw
forward
global type w_detalle_cajas from window
end type
type dw_1 from datawindow within w_detalle_cajas
end type
end forward

global type w_detalle_cajas from window
integer width = 3703
integer height = 1004
boolean titlebar = true
string title = "Detalle Cajas"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "AppIcon!"
boolean center = true
dw_1 dw_1
end type
global w_detalle_cajas w_detalle_cajas

on w_detalle_cajas.create
this.dw_1=create dw_1
this.Control[]={this.dw_1}
end on

on w_detalle_cajas.destroy
destroy(this.dw_1)
end on

event open;Long	ll_Fila

istr_busq		=	Message.PowerObjectParm

dw_1.SetTransObject(gt_UNITEC)

ll_Fila = dw_1.Retrieve(Integer(istr_Busq.Argum[1]), Integer(istr_Busq.Argum[2]), Long(istr_Busq.Argum[3]))
end event

type dw_1 from datawindow within w_detalle_cajas
integer x = 37
integer y = 28
integer width = 3607
integer height = 856
integer taborder = 10
string title = "none"
string dataobject = "dw_mues_detallecajas"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;If Row > 0 Then
	This.SelectRow(0,False)
	This.SetRow(Row)
	This.SelectRow(Row,True)
	
	dw_1.ScrollToRow(Row)
	dw_1.SetRow(Row)
End If

Return 0
end event

