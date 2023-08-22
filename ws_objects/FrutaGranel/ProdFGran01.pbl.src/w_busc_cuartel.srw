$PBExportHeader$w_busc_cuartel.srw
forward
global type w_busc_cuartel from w_busqueda
end type
end forward

global type w_busc_cuartel from w_busqueda
integer x = 123
integer y = 304
integer width = 2834
string title = "Búsqueda de Cuartel"
end type
global w_busc_cuartel w_busc_cuartel

type variables

end variables

on w_busc_cuartel.create
call super::create
end on

on w_busc_cuartel.destroy
call super::destroy
end on

event open;call super::open;Integer 	li_predio
String	busca	
Long		ll_productor

dw_1.SetTransObject(Sqlca)

istr_busq		=	Message.PowerObjectParm
ll_productor =	Long(istr_busq.argum[1])
li_predio		=  Integer(istr_busq.argum[2])


istr_busq.argum[1]	= ""
istr_busq.argum[2]	= ""
istr_busq.argum[3]	= ""
istr_busq.argum[4]	= ""
istr_busq.argum[5]	= ""
istr_busq.argum[6]	= ""
istr_busq.argum[7]	= ""

is_ordena = 'Código Predio:prpr_codigo,Nombre Predio:prpr_nombre,Código Cuartel:prcc_codigo,Nombre Cuartel:prcc_nombre'

IF dw_1.Retrieve(ll_productor,li_predio) > 0 THEN	
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	CloseWithReturn(This,istr_busq)
END IF
end event

event ue_asignacion;istr_busq.argum[1]	= String(dw_1.Object.prcc_codigo[dw_1.GetRow()])
istr_busq.argum[2]	= String(dw_1.Object.prcc_nombre[dw_1.GetRow()])


CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_cuartel
integer x = 2414
end type

type dw_1 from w_busqueda`dw_1 within w_busc_cuartel
integer x = 73
integer y = 732
integer width = 2080
string dataobject = "dw_busc_spro_cuartel"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_cuartel
integer x = 2414
integer y = 1372
alignment htextalign = center!
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_cuartel
integer x = 69
integer y = 64
integer width = 2085
integer selectedtab = 2
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
boolean visible = false
integer width = 2048
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
integer width = 2048
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2048
string text = "Búsqueda          "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 393
end type

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 393
integer width = 197
end type

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
string text = "Còdigo"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1861
integer y = 316
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
		ll_fila	=	dw_1.Find(is_busca + " >= '" + is_argume + "'", 1, dw_1.RowCount())
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

