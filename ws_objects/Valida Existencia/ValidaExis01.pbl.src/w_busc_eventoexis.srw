$PBExportHeader$w_busc_eventoexis.srw
forward
global type w_busc_eventoexis from w_busqueda
end type
end forward

global type w_busc_eventoexis from w_busqueda
integer x = 1075
integer y = 474
integer width = 2734
integer height = 1752
string title = "Busca Sub Familia"
end type
global w_busc_eventoexis w_busc_eventoexis

on w_busc_eventoexis.create
call super::create
end on

on w_busc_eventoexis.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.Argum[1]	= String(dw_1.Object.evex_tipova[dw_1.GetRow()])
istr_busq.Argum[2]	= String(dw_1.Object.evex_codigo[dw_1.GetRow()])
istr_busq.Argum[3]	= dw_1.Object.evex_nombre[dw_1.GetRow()]

CloseWithReturn(This, istr_busq)
end event

event open;call super::open;istr_busq	=	Message.PowerObjectParm

is_ordena = 'Tipo:evex_tipova;Codigo:evex_codigo'

If dw_1.Retrieve() > 0 Then
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
Else
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	CloseWithReturn(This,istr_busq)
End If

end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_eventoexis
integer x = 2391
integer y = 1008
end type

event pb_insertar::clicked;call super::clicked;CloseWithReturn(parent, istr_mant)
end event

type dw_1 from w_busqueda`dw_1 within w_busc_eventoexis
string dataobject = "dw_mues_eventoexis"
boolean hscrollbar = false
end type

event dw_1::doubleclicked;call super::doubleclicked;IF row > 0 THEN
	Parent.PostEvent("ue_asignacion")
END IF
end event

type pb_salir from w_busqueda`pb_salir within w_busc_eventoexis
integer x = 2386
integer y = 1312
end type

event pb_salir::clicked;CloseWithReturn(parent, istr_mant)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_eventoexis
integer width = 2094
integer height = 620
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer x = 18
integer y = 112
integer height = 492
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
end type

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer x = 18
integer y = 112
integer height = 492
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer x = 18
integer y = 112
integer height = 492
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
end type

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
end type

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
end type

