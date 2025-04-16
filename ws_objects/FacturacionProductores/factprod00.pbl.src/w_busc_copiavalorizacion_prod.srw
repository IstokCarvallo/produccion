$PBExportHeader$w_busc_copiavalorizacion_prod.srw
forward
global type w_busc_copiavalorizacion_prod from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type uo_selcliente from uo_seleccion_clientesprod within tabpage_1
end type
type st_3 from statictext within tabpage_1
end type
type em_produc from editmask within tabpage_1
end type
type cb_2 from uo_buscar within tabpage_1
end type
type sle_nompro from singlelineedit within tabpage_1
end type
type sle_nomzona from singlelineedit within tabpage_1
end type
type uo_selespecie from uo_seleccion_especie within tabpage_1
end type
type st_4 from statictext within tabpage_1
end type
type pb_todos from picturebutton within w_busc_copiavalorizacion_prod
end type
type pb_ninguno from picturebutton within w_busc_copiavalorizacion_prod
end type
type dw_4 from uo_dw within w_busc_copiavalorizacion_prod
end type
end forward

global type w_busc_copiavalorizacion_prod from w_busqueda
integer width = 4110
integer height = 2288
string title = "Copia de Valores Productor"
pb_todos pb_todos
pb_ninguno pb_ninguno
dw_4 dw_4
end type
global w_busc_copiavalorizacion_prod w_busc_copiavalorizacion_prod

type variables
uo_semanafactura	iuo_Semana
uo_Productores		iuo_Productor
end variables

on w_busc_copiavalorizacion_prod.create
int iCurrent
call super::create
this.pb_todos=create pb_todos
this.pb_ninguno=create pb_ninguno
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_todos
this.Control[iCurrent+2]=this.pb_ninguno
this.Control[iCurrent+3]=this.dw_4
end on

on w_busc_copiavalorizacion_prod.destroy
call super::destroy
destroy(this.pb_todos)
destroy(this.pb_ninguno)
destroy(this.dw_4)
end on

event open;call super::open;Long		ll_fila

dw_1.SetRedraw(False)

istr_Mant		=	Message.PowerObjectParm

Tab_1.TabPage_1.uo_SelCliente.Seleccion(False, False)
Tab_1.TabPage_1.uo_SelEspecie.Seleccion(False, False)

iuo_Semana		=	Create uo_semanafactura
iuo_Productor	=	Create uo_Productores	

is_ordena = 'Especie:espe_codigo,Variedad:vari_codigo,Embalaje:emba_codigo,Tipo Vida:emba_tipvid,Calibre:vaca_calibr,Semana:semana'

If UpperBound(istr_Mant.Argumento) > 0 Then
	If istr_Mant.Argumento[1] <> "" Then
		Tab_1.TabPage_1.uo_SelCliente.Inicia(Integer(istr_Mant.Argumento[1]))
		Tab_1.TabPage_1.uo_SelEspecie.Inicia(Integer(istr_Mant.Argumento[5]))

		dw_1.SetFocus()
	End If
End If

ll_Fila = dw_1.Retrieve(Tab_1.TabPage_1.uo_SelCliente.Codigo, Long(Tab_1.TabPage_1.em_Produc.Text), Tab_1.TabPage_1.uo_SelEspecie.Codigo)

If ll_Fila > 0 Then
	For ll_Fila = 1 To dw_1.RowCount()
		If iuo_Semana.of_Semana(dw_1.Object.vafa_fecini[ll_Fila], SQLCA) Then
			dw_1.Object.semana[ll_Fila] = iuo_Semana.Semana
		End If
	Next
End If

dw_4.SetTransObject(SqlCa)
istr_Mant.dw.ShareData(dw_4)

dw_1.SelectRow(1,True)
dw_1.SetRedraw(True)


end event

event ue_asignacion;istr_busq.argum[1]	= String(dw_1.Object.clie_codigo[dw_1.GetRow()])
istr_busq.argum[2]	= String(iuo_Productor.Zona)
istr_busq.argum[3]	= String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_busq.argum[4]	= String(dw_1.Object.vari_codigo[dw_1.GetRow()])
istr_busq.argum[5]	= dw_1.Object.vaca_calibr[dw_1.GetRow()]
istr_busq.argum[6]	= String(dw_1.Object.vafp_preuni[dw_1.GetRow()])
istr_busq.argum[7]	= String(dw_1.Object.vafa_fecini[dw_1.GetRow()], "dd/mm/yyyy")
istr_busq.argum[8]	= String(dw_1.Object.vafa_fecter[dw_1.GetRow()], "dd/mm/yyyy")
istr_busq.argum[9]	= String(dw_1.Object.vafa_secuen[dw_1.GetRow()])
istr_busq.argum[10]	= dw_1.Object.emba_codigo[dw_1.GetRow()]
istr_busq.argum[11]	= String(dw_1.Object.todos[dw_1.GetRow()])
istr_busq.argum[12]	= String(dw_1.Object.todoscal[dw_1.GetRow()])
istr_busq.argum[13]	= String(dw_1.Object.emba_tipvid[dw_1.GetRow()])
istr_busq.argum[14]	= String(dw_1.Object.semana[dw_1.GetRow()])
istr_busq.argum[15]	= dw_1.Object.colo_nombre[dw_1.GetRow()]

CloseWithReturn(This, istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_copiavalorizacion_prod
integer x = 3703
integer y = 744
integer taborder = 50
boolean cancel = false
boolean default = true
end type

event pb_insertar::clicked;call super::clicked;Long 				ll_Fila, ll_New, ll_Busqueda
String				ls_Busqueda
Str_Busqueda	lstr_Busq
uo_Variedades	luo_Variedad

luo_Variedad	=	Create uo_Variedades

For ll_Fila = 1 To dw_1.RowCount()
	If dw_1.IsSelected(ll_Fila) Then
		
		ls_Busqueda = "espe_codigo = " + String(dw_1.Object.espe_codigo[ll_Fila]) + " and " + "vari_codigo = " + String(dw_1.Object.vari_codigo[ll_Fila]) + " and " + &
							"String(vafa_fecini,'dd/mm/yyyy') = '" + String(dw_1.Object.vafa_fecini[ll_Fila], "dd/mm/yyyy") + "' and " + &
							"emba_codigo = '" + dw_1.Object.emba_codigo[ll_Fila] + "'" +" and  emba_tipvid = " + String(dw_1.Object.emba_tipvid[ll_Fila]) + " and " + &
							"colo_nombre = '" + dw_1.Object.colo_nombre[ll_Fila] + "'" +" and " + &
							"String(vafa_fecter,'dd/mm/yyyy') = '" + String(dw_1.Object.vafa_fecter[ll_Fila], "dd/mm/yyyy") + "' and " + &
							"vaca_calibr = '" + dw_1.Object.vaca_calibr[ll_Fila] + "'"

		ll_Busqueda = dw_4.Find(ls_Busqueda, 1, dw_4.RowCount())
		
		If ll_Busqueda = 0 Then		
			ll_New = dw_4.InsertRow(0)
			
			luo_Variedad.Existe(dw_1.Object.espe_codigo[ll_Fila], dw_1.Object.vari_codigo[ll_Fila], False, SQLCA)
			
			dw_4.Object.clie_codigo[ll_New]		=	dw_1.Object.clie_codigo[ll_Fila]
			dw_4.Object.prod_codigo[ll_New]		=	Long(istr_Mant.Argumento[2])
			dw_4.Object.espe_codigo[ll_New]		=	dw_1.Object.espe_codigo[ll_Fila]
			dw_4.Object.vari_codigo[ll_New]		=	dw_1.Object.vari_codigo[ll_Fila]
			dw_4.Object.vari_nombre[ll_New]		=	luo_Variedad.NombreVariedad
			dw_4.Object.vaca_calibr[ll_New]		=	dw_1.Object.vaca_calibr[ll_Fila]
			dw_4.Object.vafp_preuni[ll_New]		=	dw_1.Object.vafp_preuni[ll_Fila]
			dw_4.Object.vafa_fecini[ll_New]		=	dw_1.Object.vafa_fecini[ll_Fila]
			dw_4.Object.vafa_fecter[ll_New]		=	dw_1.Object.vafa_fecter[ll_Fila]
			dw_4.Object.emba_codigo[ll_New]	=	dw_1.Object.emba_codigo[ll_Fila]
			dw_4.Object.todos[ll_New]				=	dw_1.Object.todos[ll_Fila]
			dw_4.Object.todoscal[ll_New]			=	dw_1.Object.todoscal[ll_Fila]
			dw_4.Object.emba_tipvid[ll_New]		=	dw_1.Object.emba_tipvid[ll_Fila]
			dw_4.Object.semana[ll_New]			=	dw_1.Object.semana[ll_Fila]
			dw_4.Object.colo_nombre[ll_New]	=	dw_1.Object.colo_nombre[ll_Fila]
		End If
	End If
Next

Destroy luo_Variedad

CloseWithReturn(Parent, lstr_Busq)
end event

type dw_1 from w_busqueda`dw_1 within w_busc_copiavalorizacion_prod
integer x = 114
integer y = 792
integer width = 3515
integer height = 1308
integer taborder = 60
string dataobject = "dw_mues_valofactprod"
end type

event dw_1::doubleclicked;IF row > 0 THEN
	Parent.PostEvent("ue_asignacion")
END IF
end event

event dw_1::clicked;Long	ll_Fila, ll_SelectedRow

If Row = 0 Then
	/*
	Para que funcione este ordenamiento los títulos deben tener el nombre
	de la columna y terminacion "_t", de lo contrario no funcionará
	*/
	String		ls_old_sort, ls_column, ls_color_old
	Char		lc_sort
	
	IF IsNull(dwo) THEN RETURN
	
	If Right(dwo.Name,2) = '_t' Then
		ls_column	= Left (dwo.Name, Len(String(dwo.Name)) - 2)
		ls_old_sort	= This.Describe("Datawindow.Table.sort")
		ls_color_old	=This.Describe(ls_Column + "_t.Color")
	
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
			This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = " + ls_color_old)
		End If
		
		This.Modify(dwo.Name + ".Color = " + String(Rgb(0, 0, 255)))
		
		This.Sort()
	End If

Else
	ll_SelectedRow = This.GetSelectedRow(0)
	
	If KeyDown(keyShIft!) Then
		If ll_SelectedRow = 0 Then
			This.SelectRow(Row, True)
		Else
			This.SelectRow(1, False)
			If Row > ll_SelectedRow Then
				For ll_Fila = ll_SelectedRow To Row
					This.SelectRow(ll_Fila, True)
				Next
			Else
				For ll_Fila = Row To ll_SelectedRow
					This.SelectRow(ll_Fila, True)
				Next
			End If
		End If
	
	ElseIf KeyDown(keyControl!) Then
		If This.IsSelected(Row) Then
			This.SelectRow(Row, False)
		Else
			This.SelectRow(Row, True)
		End If
	
	Else
		If This.IsSelected(Row) Then
			This.SelectRow(0, False)
			This.SelectRow(Row, True)
		Else
			This.SelectRow(0, False)
			This.SelectRow(Row, True)
		End If
	End If
End If
end event

type pb_salir from w_busqueda`pb_salir within w_busc_copiavalorizacion_prod
integer x = 3698
integer y = 1052
end type

event pb_salir::clicked;
CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_copiavalorizacion_prod
integer width = 2761
end type

on tab_1.create
call super::create
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3}
end on

on tab_1.destroy
call super::destroy
end on

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 2725
st_1 st_1
st_2 st_2
uo_selcliente uo_selcliente
st_3 st_3
em_produc em_produc
cb_2 cb_2
sle_nompro sle_nompro
sle_nomzona sle_nomzona
uo_selespecie uo_selespecie
st_4 st_4
end type

on tabpage_1.create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selcliente=create uo_selcliente
this.st_3=create st_3
this.em_produc=create em_produc
this.cb_2=create cb_2
this.sle_nompro=create sle_nompro
this.sle_nomzona=create sle_nomzona
this.uo_selespecie=create uo_selespecie
this.st_4=create st_4
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.em_produc
this.Control[iCurrent+6]=this.cb_2
this.Control[iCurrent+7]=this.sle_nompro
this.Control[iCurrent+8]=this.sle_nomzona
this.Control[iCurrent+9]=this.uo_selespecie
this.Control[iCurrent+10]=this.st_4
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selcliente)
destroy(this.st_3)
destroy(this.em_produc)
destroy(this.cb_2)
destroy(this.sle_nompro)
destroy(this.sle_nomzona)
destroy(this.uo_selespecie)
destroy(this.st_4)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2066
integer y = 272
end type

event pb_filtrar::clicked;call super::clicked;Long ll_Fila

ll_Fila = dw_1.Retrieve(Tab_1.TabPage_1.uo_SelCliente.Codigo, Long(Tab_1.TabPage_1.em_Produc.Text), Tab_1.TabPage_1.uo_SelEspecie.Codigo)

If ll_Fila > 0 Then
	For ll_Fila = 1 To dw_1.RowCount()
		If iuo_Semana.of_Semana(dw_1.Object.vafa_fecini[ll_Fila], SQLCA) Then
			dw_1.Object.semana[ll_Fila] = iuo_Semana.Semana
		End If
	Next
End If
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2725
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
boolean visible = false
integer width = 2725
boolean enabled = false
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer y = 244
end type

event sle_argumento2::getfocus;This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(166,180,210)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "nave_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 114
integer y = 252
integer width = 311
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer y = 132
integer width = 210
end type

event sle_argumento1::getfocus;This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "nave_codigo"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 114
integer y = 140
integer width = 311
string text = "Especie"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
end type

type st_1 from statictext within tabpage_1
integer x = 78
integer y = 116
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_1
integer x = 78
integer y = 360
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Zona"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within tabpage_1
integer x = 457
integer y = 100
integer height = 108
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_3 from statictext within tabpage_1
integer x = 78
integer y = 240
integer width = 297
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type em_produc from editmask within tabpage_1
integer x = 457
integer y = 236
integer width = 219
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
string displaydata = ""
end type

event modified;If This.Text =  '' Then Return 

If Not iuo_Productor.Existe(Long(This.Text), True, Sqlca) Then 
	sle_nompro.text	=	""
	sle_nomzona.text	=	""
	This.Text				=	""
	This.SetFocus()
Else
	sle_nompro.text			= iuo_Productor.Nombre
	sle_nomzona.text			= iuo_Productor.NombreZona
End If
end event

type cb_2 from uo_buscar within tabpage_1
integer x = 695
integer y = 240
integer width = 96
integer height = 84
integer taborder = 40
boolean bringtotop = true
end type

event clicked;istr_busq.argum[1]  = ''

OpenWithParm(w_busc_productores, istr_busq)

istr_busq	= Message.PowerObjectParm

If istr_busq.argum[1] = "" Then
	em_produc.SetFocus()
Else
	em_produc.Text			= istr_busq.argum[1]
	sle_nompro.Text			= istr_busq.argum[2]
	sle_nomzona.Text			= istr_busq.argum[4]

	istr_mant.argumento[2]	= istr_busq.argum[1]
	istr_mant.argumento[3]	= istr_busq.argum[2]
End If
end event

type sle_nompro from singlelineedit within tabpage_1
integer x = 809
integer y = 236
integer width = 1211
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_nomzona from singlelineedit within tabpage_1
integer x = 457
integer y = 352
integer width = 1563
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type uo_selespecie from uo_seleccion_especie within tabpage_1
integer x = 1778
integer y = 100
integer height = 108
integer taborder = 30
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_4 from statictext within tabpage_1
integer x = 1435
integer y = 124
integer width = 261
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type pb_todos from picturebutton within w_busc_copiavalorizacion_prod
integer x = 2930
integer y = 284
integer width = 494
integer height = 180
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\btn_todos_on.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\btn_todos_off.png"
alignment htextalign = left!
end type

event clicked;dw_1.SetRedraw(False)
dw_1.SelectRow(0,True)
dw_1.SetRedraw(True)
end event

type pb_ninguno from picturebutton within w_busc_copiavalorizacion_prod
integer x = 2930
integer y = 504
integer width = 494
integer height = 180
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\btn_ninguno_on.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\btn_ninguno_off.png"
alignment htextalign = left!
end type

event clicked;dw_1.SetRedraw(False)
dw_1.SelectRow(0,False)
dw_1.SetRedraw(True)
end event

type dw_4 from uo_dw within w_busc_copiavalorizacion_prod
boolean visible = false
integer x = 3621
integer y = 20
integer width = 352
integer height = 240
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_valofactprod"
boolean vscrollbar = false
end type

