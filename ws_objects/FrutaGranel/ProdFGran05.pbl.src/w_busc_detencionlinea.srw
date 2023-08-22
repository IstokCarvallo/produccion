$PBExportHeader$w_busc_detencionlinea.srw
forward
global type w_busc_detencionlinea from w_busqueda
end type
end forward

global type w_busc_detencionlinea from w_busqueda
integer x = 123
integer y = 304
integer width = 2734
string title = "Búsqueda Detención Línea"
end type
global w_busc_detencionlinea w_busc_detencionlinea

type variables
DataWindowChild		idwc_especie
end variables

on w_busc_detencionlinea.create
call super::create
end on

on w_busc_detencionlinea.destroy
call super::destroy
end on

event open;call super::open;String	busca


istr_busq.argum[1]	= ""
istr_busq.argum[2]	= ""

is_ordena = 'Código Planta:plde_codigo,Línea:line_codigo,Especie:espe_codigo,Fecha Proceso:edla_fecpro'

dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
ELSE
	idwc_especie.InsertRow(0)
END IF

IF dw_1.Retrieve(0) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	CloseWithReturn(This,istr_busq)
END IF
end event

event ue_asignacion();Date    ldt_fecini,ldt_horini

ldt_fecini = dw_1.Object.edla_fecpro[dw_1.GetRow()]

istr_busq.argum[1]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"plde_codigo"))
istr_busq.argum[2]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"line_codigo"))
istr_busq.argum[3]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"espe_codigo"))
istr_busq.argum[4]	= String(ldt_fecini)
istr_busq.argum[5]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"rdla_turno"))
CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_detencionlinea
integer x = 2414
integer y = 1108
end type

type dw_1 from w_busqueda`dw_1 within w_busc_detencionlinea
integer x = 73
integer y = 732
integer width = 2199
string dataobject = "dw_busc_spro_detencionlinea"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_detencionlinea
integer x = 2414
integer y = 1372
alignment htextalign = center!
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_detencionlinea
integer x = 142
integer y = 64
integer width = 2085
integer selectedtab = 2
end type

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

event sle_argumento1::modified;call super::modified;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "line_codigo"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
string text = "Línea"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1861
integer y = 316
end type

event pb_buscar::clicked;call super::clicked;Long		ll_fila
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

