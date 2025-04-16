$PBExportHeader$w_busc_copiavalorizacion.srw
forward
global type w_busc_copiavalorizacion from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type uo_selcliente from uo_seleccion_clientesprod within tabpage_1
end type
type uo_selzonas from uo_seleccion_zonas within tabpage_1
end type
type uo_selespecie from uo_seleccion_especie within tabpage_1
end type
type st_3 from statictext within tabpage_1
end type
type pb_todos from picturebutton within w_busc_copiavalorizacion
end type
type pb_ninguno from picturebutton within w_busc_copiavalorizacion
end type
type dw_4 from uo_dw within w_busc_copiavalorizacion
end type
end forward

global type w_busc_copiavalorizacion from w_busqueda
integer width = 3739
integer height = 2192
string title = "Copia de Valores Exportadora"
pb_todos pb_todos
pb_ninguno pb_ninguno
dw_4 dw_4
end type
global w_busc_copiavalorizacion w_busc_copiavalorizacion

type variables
uo_semanafactura	iuo_Semana
end variables

on w_busc_copiavalorizacion.create
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

on w_busc_copiavalorizacion.destroy
call super::destroy
destroy(this.pb_todos)
destroy(this.pb_ninguno)
destroy(this.dw_4)
end on

event open;call super::open;Long		ll_fila

dw_1.SetRedraw(False)

istr_Mant		=	Message.PowerObjectParm

Tab_1.TabPage_1.uo_SelCliente.Seleccion(False, False)
Tab_1.TabPage_1.uo_SelZonas.Seleccion(False, False)
Tab_1.TabPage_1.uo_SelEspecie.Seleccion(False, False)

iuo_Semana	=	Create uo_semanafactura

is_ordena = 'Especie:espe_codigo,Variedad:vari_codigo,Embalaje:emba_codigo,Tipo Vida:emba_tipvid,Calibre:vaca_calibr,Semana:semana'

If UpperBound(istr_Mant.Argumento) > 0 Then
	If istr_Mant.Argumento[1] <> "" Then
		Tab_1.TabPage_1.uo_SelCliente.Inicia(Integer(istr_Mant.Argumento[1]))
		Tab_1.TabPage_1.uo_SelZonas.Inicia(Integer(istr_Mant.Argumento[4]))
		Tab_1.TabPage_1.uo_SelEspecie.Inicia(Integer(istr_Mant.Argumento[5]))
		
		dw_1.SetFocus()
	End If
End If

ll_Fila = dw_1.Retrieve(Tab_1.TabPage_1.uo_SelCliente.Codigo, Tab_1.TabPage_1.uo_SelZonas.Codigo, Tab_1.TabPage_1.uo_SelEspecie.Codigo)

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
istr_busq.argum[2]	= String(dw_1.Object.zona_codigo[dw_1.GetRow()])
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

type pb_insertar from w_busqueda`pb_insertar within w_busc_copiavalorizacion
integer x = 3351
integer y = 732
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

type dw_1 from w_busqueda`dw_1 within w_busc_copiavalorizacion
integer width = 3141
integer height = 1308
integer taborder = 60
string dataobject = "dw_mues_pmg_exportadora"
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

type pb_salir from w_busqueda`pb_salir within w_busc_copiavalorizacion
integer x = 3346
integer y = 1040
end type

event pb_salir::clicked;
CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_copiavalorizacion
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
st_1 st_1
st_2 st_2
uo_selcliente uo_selcliente
uo_selzonas uo_selzonas
uo_selespecie uo_selespecie
st_3 st_3
end type

on tabpage_1.create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selcliente=create uo_selcliente
this.uo_selzonas=create uo_selzonas
this.uo_selespecie=create uo_selespecie
this.st_3=create st_3
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.uo_selzonas
this.Control[iCurrent+5]=this.uo_selespecie
this.Control[iCurrent+6]=this.st_3
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selcliente)
destroy(this.uo_selzonas)
destroy(this.uo_selespecie)
destroy(this.st_3)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
end type

event pb_filtrar::clicked;call super::clicked;Long ll_Fila

ll_Fila = dw_1.Retrieve(Tab_1.TabPage_1.uo_SelCliente.Codigo, Tab_1.TabPage_1.uo_SelZonas.Codigo, Tab_1.TabPage_1.uo_SelEspecie.Codigo)

If ll_Fila > 0 Then
	For ll_Fila = 1 To dw_1.RowCount()
		If iuo_Semana.of_Semana(dw_1.Object.vafa_fecini[ll_Fila], SQLCA) Then
			dw_1.Object.semana[ll_Fila] = iuo_Semana.Semana
		End If
	Next
End If
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
boolean visible = false
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
integer x = 96
integer y = 80
integer width = 466
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
integer x = 96
integer y = 220
integer width = 466
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
integer x = 640
integer y = 64
integer height = 108
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selzonas from uo_seleccion_zonas within tabpage_1
integer x = 640
integer y = 204
integer height = 108
integer taborder = 21
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas::destroy
end on

type uo_selespecie from uo_seleccion_especie within tabpage_1
integer x = 640
integer y = 344
integer height = 108
integer taborder = 30
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_3 from statictext within tabpage_1
integer x = 96
integer y = 364
integer width = 457
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type pb_todos from picturebutton within w_busc_copiavalorizacion
integer x = 2409
integer y = 356
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

type pb_ninguno from picturebutton within w_busc_copiavalorizacion
integer x = 2409
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

type dw_4 from uo_dw within w_busc_copiavalorizacion
boolean visible = false
integer x = 2432
integer y = 52
integer width = 352
integer height = 240
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_valofactprod"
boolean vscrollbar = false
end type

