$PBExportHeader$w_busc_spro_ordenproceso.srw
forward
global type w_busc_spro_ordenproceso from w_busqueda
end type
type dw_5 from datawindow within tabpage_1
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type rb_vigente from radiobutton within tabpage_1
end type
type rb_cerrada from radiobutton within tabpage_1
end type
type rb_ambos from radiobutton within tabpage_1
end type
type rb_confir from radiobutton within tabpage_1
end type
end forward

global type w_busc_spro_ordenproceso from w_busqueda
integer x = 123
integer y = 304
integer width = 2994
string title = "Busca Ordenes de Proceso"
end type
global w_busc_spro_ordenproceso w_busc_spro_ordenproceso

type variables
datawindowchild  idwc_planta

integer ii_estado = -1
end variables

event open;call super::open;istr_busq				=	Message.PowerObjectParm

dw_1.DataObject	=	"dw_mues_ordenesproceso"
dw_1.SetTransObject(Sqlca)
is_ordena 			=	'Orden:orpr_numero,Productor:prod_codigo,Especie:espe_codigo'

dw_1.SetTransObject(sqlca)

Tab_1.TabPage_1.dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)

If idwc_planta.Retrieve() = 0 Then MessageBox("Atención", "Falta Registrar Plantas")
Tab_1.TabPage_1.dw_5.SetTransObject(SQLCA)
Tab_1.TabPage_1.dw_5.InsertRow(0)

If istr_Busq.Argum[1] <> "" Then
	Tab_1.TabPage_1.dw_5.SetItem(1, "plde_codigo", Integer(istr_Busq.Argum[1]))
	Tab_1.TabPage_1.dw_5.Enabled	=	False
End If

Tab_1.TabPage_1.pb_Filtrar.SetFocus()

If dw_1.Retrieve(Integer(istr_Busq.Argum[1]),Integer(istr_Busq.Argum[2]),ii_Estado,Integer(istr_Busq.Argum[4])) > 0 Then
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
End If
end event

on w_busc_spro_ordenproceso.create
int iCurrent
call super::create
end on

on w_busc_spro_ordenproceso.destroy
call super::destroy
end on

event ue_asignacion;istr_Busq.Argum[3]	=	String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_Busq.Argum[4]	=	String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_Busq.Argum[5]	=	String(dw_1.Object.orpr_fecpro[dw_1.GetRow()],"dd/mm/yyyy")
istr_Busq.Argum[6]	=	String(dw_1.Object.orpr_numero[dw_1.GetRow()])
istr_Busq.Argum[7]	=	String(dw_1.Object.prod_codigo[dw_1.GetRow()])
istr_Busq.Argum[8]	=	String(dw_1.Object.vari_codigo[dw_1.GetRow()])
istr_Busq.Argum[9]	=	String(dw_1.Object.orpr_canbul[dw_1.GetRow()])
istr_Busq.Argum[10]	=	String(dw_1.Object.orpr_tipord[dw_1.GetRow()])
istr_Busq.Argum[11]	=	String(dw_1.Object.ppre_numero[dw_1.GetRow()])
istr_Busq.Argum[12]	=	String(dw_1.Object.line_codigo[dw_1.GetRow()])
istr_Busq.Argum[13]	=	String(dw_1.Object.orpr_nrotur[dw_1.GetRow()])
istr_Busq.Argum[14]	=	dw_1.Object.frio_tipofr[dw_1.GetRow()]
istr_Busq.Argum[15]	=	String(dw_1.Object.pefr_codigo[dw_1.GetRow()])
istr_Busq.Argum[16]	=	String(dw_1.Object.clie_codigo[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

event ue_ordenamiento;Integer li_posi, li_idx
String  ls_temp, ls_buscar_info, tmp

setpointer(hourglass!)
w_cargacajas_systray.SetMicroHelp("Cargando Ordenamiento...")

li_idx = 1
ls_buscar_info = is_ordena

IF is_ordena <> "" THEN 
	DO 
		li_posi = pos(ls_buscar_info,",")
		IF li_posi = 0 THEN
			ls_temp = ls_buscar_info 
			ls_buscar_info = ""
		ELSE
			ls_temp = left(ls_buscar_info, li_posi - 1)
			ls_buscar_info = mid(ls_buscar_info, li_posi+1)
		END IF
		li_posi = pos(ls_temp, ":")
		tab_1.TabPage_2.dw_2.insertrow(li_idx)
		tab_1.TabPage_2.dw_2.setitem(li_idx,"campo",mid(ls_temp, li_posi + 1))
		tab_1.TabPage_2.dw_2.setitem(li_idx,"nombre",left(ls_temp, li_posi - 1))
		li_idx = li_idx + 1
	LOOP UNTIL ls_buscar_info = "" or li_idx = 10  
	tab_1.TabPage_2.dw_2.setsort("nombre A")
	tab_1.TabPage_2.dw_2.sort()
END IF
w_cargacajas_systray.SetMicroHelp("Listo")

end event

event mousemove;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_spro_ordenproceso
boolean visible = false
integer x = 2601
integer y = 928
boolean enabled = false
boolean cancel = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_spro_ordenproceso
integer width = 2423
string dataobject = "dw_mues_ordenesproceso"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_spro_ordenproceso
integer x = 2601
integer y = 1320
end type

event pb_salir::clicked;istr_busq.argum[1] = ""
istr_busq.argum[2] = ""
istr_busq.argum[3] = ""

CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_spro_ordenproceso
integer x = 73
integer width = 2199
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
integer width = 2162
dw_5 dw_5
st_1 st_1
st_2 st_2
rb_vigente rb_vigente
rb_cerrada rb_cerrada
rb_ambos rb_ambos
rb_confir rb_confir
end type

on tabpage_1.create
this.dw_5=create dw_5
this.st_1=create st_1
this.st_2=create st_2
this.rb_vigente=create rb_vigente
this.rb_cerrada=create rb_cerrada
this.rb_ambos=create rb_ambos
this.rb_confir=create rb_confir
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_5
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.rb_vigente
this.Control[iCurrent+5]=this.rb_cerrada
this.Control[iCurrent+6]=this.rb_ambos
this.Control[iCurrent+7]=this.rb_confir
end on

on tabpage_1.destroy
call super::destroy
destroy(this.dw_5)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.rb_vigente)
destroy(this.rb_cerrada)
destroy(this.rb_ambos)
destroy(this.rb_confir)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer y = 324
end type

event pb_filtrar::clicked;call super::clicked;
IF dw_1.Retrieve(Integer(istr_Busq.Argum[1]),Integer(istr_Busq.Argum[2]),ii_Estado,Integer(istr_Busq.Argum[4])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2162
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2162
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
end type

event sle_argumento2::modified;call super::modified;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(166,180,210)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "prod_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
string text = "Productor"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
end type

event sle_argumento1::modified;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0

es_numero						= True
is_busca							= "dinp_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
string text = "Orden Proc."
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
end type

type dw_5 from datawindow within tabpage_1
integer x = 635
integer y = 96
integer width = 873
integer height = 84
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "plde_codigo"
		istr_Busq.Argum[1]	=	data
END CHOOSE
end event

type st_1 from statictext within tabpage_1
integer x = 229
integer y = 100
integer width = 288
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_1
integer x = 229
integer y = 284
integer width = 288
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Estado"
boolean focusrectangle = false
end type

type rb_vigente from radiobutton within tabpage_1
integer x = 626
integer y = 228
integer width = 357
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Vigente"
end type

event clicked;ii_Estado	= 1
end event

type rb_cerrada from radiobutton within tabpage_1
integer x = 622
integer y = 320
integer width = 357
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Cerrada"
end type

event clicked;ii_Estado	= 3
end event

type rb_ambos from radiobutton within tabpage_1
integer x = 1120
integer y = 320
integer width = 357
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;ii_Estado	= -1
end event

type rb_confir from radiobutton within tabpage_1
integer x = 1125
integer y = 236
integer width = 526
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Confir. Packing"
end type

event clicked;ii_Estado	= 2	
end event

