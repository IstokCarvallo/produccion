$PBExportHeader$w_busc_prodpredio.srw
forward
global type w_busc_prodpredio from w_busqueda
end type
type st_argum3 from statictext within tabpage_3
end type
type sle_argumento3 from singlelineedit within tabpage_3
end type
end forward

global type w_busc_prodpredio from w_busqueda
integer x = 123
integer y = 304
integer width = 2939
string title = "Búsqueda de predios"
end type
global w_busc_prodpredio w_busc_prodpredio

type variables

end variables

on w_busc_prodpredio.create
int iCurrent
call super::create
end on

on w_busc_prodpredio.destroy
call super::destroy
end on

event open;call super::open;es_numero				= True

istr_busq	= Message.PowerObjectParm

//istr_busq.argum[2]	= ""
//istr_busq.argum[3]	= ""

IF dw_1.Retrieve(Long(istr_busq.argum[2])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
	
	is_ordena = 'Código:prpr_codigo,Nombre:prpr_nombre,Dirección:prpr_direcc,Zona:zona_codigo'
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	CloseWithReturn(This,istr_busq)
END IF
end event

event ue_asignacion;istr_busq.argum[1]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"prpr_codigo"))
istr_busq.argum[2]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"prod_codigo"))
istr_busq.argum[3]	= dw_1.GetItemString(dw_1.GetRow(),"prpr_nombre")
istr_busq.argum[4]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"zona_codigo"))
istr_busq.argum[5]	= dw_1.GetItemString(dw_1.GetRow(),"prod_nombre")
CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_prodpredio
integer x = 2610
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_prodpredio
integer y = 740
integer width = 2405
integer taborder = 60
string dataobject = "dw_busc_prodpredio"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_prodpredio
integer x = 2610
integer y = 1420
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_prodpredio
integer x = 133
integer y = 60
integer width = 2098
integer selectedtab = 2
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
boolean visible = false
integer width = 2062
boolean enabled = false
string text = "Filtros              "
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
boolean visible = false
integer x = 1280
boolean enabled = false
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2062
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1861
integer y = 312
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2062
string text = "Búsqueda          "
st_argum3 st_argum3
sle_argumento3 sle_argumento3
end type

on tabpage_3.create
this.st_argum3=create st_argum3
this.sle_argumento3=create sle_argumento3
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_argum3
this.Control[iCurrent+2]=this.sle_argumento3
end on

on tabpage_3.destroy
call super::destroy
destroy(this.st_argum3)
destroy(this.sle_argumento3)
end on

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 512
integer y = 196
end type

event sle_argumento2::getfocus;This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(166,180,210)
sle_argumento1.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(166,180,210)
sle_argumento3.TabOrder		= 0
es_numero						= False
is_busca							= "prpr_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 96
integer y = 204
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 512
integer y = 76
integer width = 201
end type

event sle_argumento1::getfocus;This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(166,180,210)
sle_argumento3.TabOrder		= 0
es_numero						= True
is_busca							= "prpr_codigo"

end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 96
string text = "Código"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1856
integer y = 312
end type

event pb_buscar::clicked;Long		ll_fila
Boolean	lb_inicio, lb_termino

dw_1.SetRedraw(False)

IF dw_1.RowCount() > 0 THEN
	dw_1.SetSort(is_busca + " A")
	dw_1.Sort()
	dw_1.SetFilter("")
	dw_1.Filter()

	tab_1.tabpage_2.dw_3.Reset()
	
	IF es_numero THEN
		ll_fila	=	dw_1.Find(is_busca + " >= " + is_argume, 1, dw_1.RowCount())
	ELSE
		IF Mid(is_argume, 1, 1) = "%" THEN
			lb_inicio	=	True
			is_argume	=	Mid(is_argume, 2)
		END IF
		
		IF Mid(is_argume, Len(is_argume)) = "%" THEN
			lb_termino	=	True
			is_argume	=	Mid(is_argume, 1, Len(is_argume) - 1)
		END IF
		
		IF Not lb_inicio THEN
			is_busca	=	"Mid(String(" + is_busca + "), 1, " + String(Len(is_argume)) + ")"
			ll_fila	=	dw_1.Find(is_busca + " >= '" + is_argume + "'", 1, dw_1.RowCount())
		ELSEIF lb_inicio THEN
			Is_busca	=	"Pos(Lower(String(" + is_busca + ")), '" + Lower(is_argume) + "') > 0"
			dw_1.SetFilter(Is_busca)
			dw_1.Filter()
		END IF
	END IF

	IF ll_fila = 0 THEN ll_fila = 1
	
	dw_1.SetRow(ll_fila)
	dw_1.ScrollToRow(ll_fila)
	dw_1.SelectRow(0,False)
	dw_1.SelectRow(ll_fila,True)
	dw_1.SetFocus()
	dw_1.SetRedraw(True)
END IF
end event

type st_argum3 from statictext within tabpage_3
integer x = 91
integer y = 324
integer width = 384
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 30586022
boolean enabled = false
string text = "Contacto"
boolean focusrectangle = false
end type

type sle_argumento3 from singlelineedit within tabpage_3
integer x = 512
integer y = 316
integer width = 1051
integer height = 92
integer taborder = 21
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

event getfocus;This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(166,180,210)
sle_argumento1.TabOrder		= 0
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0
es_numero						= False
is_busca							= "reci_contac"
end event

event modified;is_argume = This.Text
end event

