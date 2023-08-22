$PBExportHeader$w_consulta_categoriacondicion.srw
forward
global type w_consulta_categoriacondicion from w_para_informes
end type
type dw_1 from uo_dw within w_consulta_categoriacondicion
end type
type dw_2 from uo_dw within w_consulta_categoriacondicion
end type
type st_1 from statictext within w_consulta_categoriacondicion
end type
type pb_carga from picturebutton within w_consulta_categoriacondicion
end type
type ddlb_nota from dropdownlistbox within w_consulta_categoriacondicion
end type
end forward

global type w_consulta_categoriacondicion from w_para_informes
integer x = 14
integer y = 32
integer width = 3753
integer height = 1920
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
windowstate windowstate = maximized!
string icon = "AppIcon!"
event ue_recuperadatos ( )
event ue_asignacion ( )
dw_1 dw_1
dw_2 dw_2
st_1 st_1
pb_carga pb_carga
ddlb_nota ddlb_nota
end type
global w_consulta_categoriacondicion w_consulta_categoriacondicion

type variables
str_mant 		istr_mant

Long				il_Fila, il_Nota
end variables

event ue_recuperadatos();Long 		ll_Fila, Respuesta

Do
	ll_fila	= dw_1.Retrieve(Long(istr_Mant.Argumento[1]), Long(istr_Mant.Argumento[2]), Long(istr_Mant.Argumento[3]), Long(istr_Mant.Argumento[4]))
	
	If ll_fila = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila > 0 Then
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_carga.Enabled	= True
	Else
		
	End If
Loop While respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_asignacion();graphicobject lg_Object
datawindow lg_DW
lg_Object = GetFocus()

Choose Case TypeOf (lg_Object)
	Case DataWindow!
	lg_DW = lg_Object
	ClipBoard (lg_DW.Describe ("datawindow.selected.data"))
End Choose
end event

on w_consulta_categoriacondicion.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.dw_2=create dw_2
this.st_1=create st_1
this.pb_carga=create pb_carga
this.ddlb_nota=create ddlb_nota
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.dw_2
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.pb_carga
this.Control[iCurrent+5]=this.ddlb_nota
end on

on w_consulta_categoriacondicion.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.st_1)
destroy(this.pb_carga)
destroy(this.ddlb_nota)
end on

event open;call super::open;istr_mant	= Message.PowerObjectParm

dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)
istr_mant.dw.ShareData(dw_2)

TriggerEvent('ue_recuperadatos')
end event

type st_titulo from w_para_informes`st_titulo within w_consulta_categoriacondicion
integer x = 137
integer y = 64
integer width = 1065
integer height = 256
borderstyle borderstyle = styleraised!
end type

type pb_acepta from w_para_informes`pb_acepta within w_consulta_categoriacondicion
string tag = "Recupera Informacion"
boolean visible = false
integer x = 3461
integer y = 208
integer taborder = 200
boolean enabled = false
string picturename = "\Desarrollo\Bmp\RESCATAE.BMP"
string disabledname = "\Desarrollo\Bmp\RESCATAd.BMP"
string powertiptext = "Recupera Informacion"
end type

event pb_acepta::clicked;Parent.TriggerEvent('ue_recuperadatos')
end event

type pb_salir from w_para_informes`pb_salir within w_consulta_categoriacondicion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3461
integer y = 584
integer taborder = 210
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

event pb_salir::clicked;CloseWithReturn(Parent, istr_mant)
end event

type dw_1 from uo_dw within w_consulta_categoriacondicion
integer x = 105
integer y = 368
integer width = 3177
integer height = 1440
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string title = "Consulta de Lotes"
string dataobject = "dw_genera_calidadcondicion"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;String  ls_Tecla, ls_old_sort, ls_column
Char		lc_sort

IF KeyDown(KeyShift!) THEN
	ls_tecla = "Shift"
ELSEIF KeyDown(KeyControl!) THEN
	ls_tecla = "Control"
END IF

If Row > 0 Then
	il_fila = Row
	F_Selecciona(This, ls_tecla, Row)
End If

IF IsNull(dwo) THEN RETURN

If Right(dwo.Name,2) = '_t' Then
	ls_column = Left (dwo.Name, Len(String(dwo.Name)) - 2)
	ls_old_sort = This.Describe("Datawindow.Table.sort")

	If ls_column = Left(ls_old_sort, Len(ls_old_sort) - 2) Then
		lc_sort = Right(ls_old_sort, 1)
		If lc_sort = 'A' Then
			lc_sort = 'D'
		Else
			lc_sort = 'A'
		End If
		This.SetSort(ls_column+" "+lc_sort)
	Else
		This.SetSort(ls_column+" A")
		This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = 0")
	End If
	
	This.Modify(dwo.Name + ".Color = " + String(Rgb(0, 0, 255)))
	
	This.Sort()
End If

end event

event rbuttondown;call super::rbuttondown;GraphicObject 		lg_Object
DataWindow 		lg_DW
m_consulta_edi		l_Menu 

IF Row =	0	THEN
	Return
ELSE
	l_Menu = CREATE m_consulta_edi
	lg_Object = GetFocus()
	Choose Case TypeOf (lg_Object)
		Case DataWindow!
			lg_DW = lg_Object
			l_Menu.m_edicion.m_Copiar.Enabled = (lg_DW.Describe ("datawindow.selected.data") <> '')
			l_Menu.m_edicion.PopMenu(w_main.PointerX(),w_main.PointerY())	
	End Choose	
End If
end event

type dw_2 from uo_dw within w_consulta_categoriacondicion
boolean visible = false
integer x = 1486
integer y = 1060
integer width = 2222
integer height = 728
integer taborder = 11
boolean bringtotop = true
boolean enabled = false
string dataobject = "dw_mues_ctlcalcategoriacondiciondeta"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_consulta_categoriacondicion
integer x = 206
integer y = 156
integer width = 343
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Nota"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_carga from picturebutton within w_consulta_categoriacondicion
string tag = "Carga Informacion Seleccionada"
integer x = 3461
integer y = 396
integer width = 151
integer height = 132
integer taborder = 220
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo\Bmp\ACEPTAE.BMP"
string disabledname = "\Desarrollo\Bmp\ACEPTAd.BMP"
alignment htextalign = right!
string powertiptext = "Carga Informacion Seleccionada"
end type

event clicked;Long 	ll_Fila, ll_New, ll_Busca
String	ls_Busca

For ll_Fila = 1 To dw_1.RowCount()
	If dw_1.IsSelected(ll_Fila) Then
		
		ls_Busca = "plde_codigo = " + String(dw_1.Object.plde_codigo[ll_Fila]) + "AND  coca_packin = " + String(dw_1.Object.plde_codpak[ll_Fila]) + &
						"AND emba_codigo = '"+ dw_1.Object.emba_codigo[ll_Fila]  + "'" +   "AND  String(coca_fecemb) = '" +  String(dw_1.Object.cclo_fecemb[ll_Fila]) + "'" + &
						"AND vaca_calibr ='"  + dw_1.Object.vaca_calibr[ll_Fila] + "'"
		ll_Busca = dw_2.Find(ls_Busca, 1, dw_2.RowCount())
		
		If ll_Busca = 0 Then
			ll_New	= dw_2.InsertRow(0)
			
			dw_2.Object.plde_codigo[ll_New]		=	dw_1.Object.plde_codigo[ll_Fila]
			dw_2.Object.coca_packin[ll_New]		=	dw_1.Object.plde_codpak[ll_Fila]
			dw_2.Object.emba_codigo[ll_New]	=	dw_1.Object.emba_codigo[ll_Fila]
			dw_2.Object.coca_fecemb[ll_New]	=	dw_1.Object.cclo_fecemb[ll_Fila]
			dw_2.Object.coca_cancaj[ll_New]		=	dw_1.Object.cajas[ll_Fila]	
			dw_2.Object.vaca_calibr[ll_New]		=	dw_1.Object.vaca_calibr[ll_Fila]	
			dw_2.Object.coca_catcon[ll_New]		=	il_Nota
			dw_1.Object.cclo_catcon[ll_Fila]		=	il_Nota
		
		Else
			MessageBox('Atencion', 'Registro Nro.: ' + String(ll_Fila) + ', ya fue marcado.', StopSign!, Ok!)
		End If
	End If	
Next

//CloseWithReturn(Parent, istr_mant)
end event

type ddlb_nota from dropdownlistbox within w_consulta_categoriacondicion
integer x = 576
integer y = 140
integer width = 475
integer height = 464
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean sorted = false
boolean vscrollbar = true
string item[] = {"1","2","3","4"," "}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;Choose Case Index
	Case	1 
		il_Nota = 1
		
	Case	2
		il_Nota = 2
		
	Case	3
		il_Nota = 3
		
	Case	4
		il_Nota = 4
		
	Case	5
		SetNull(il_Nota)
		
End Choose
end event

