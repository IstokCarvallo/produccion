$PBExportHeader$w_busc_ctlcalsubfamilia.srw
forward
global type w_busc_ctlcalsubfamilia from w_busqueda
end type
end forward

global type w_busc_ctlcalsubfamilia from w_busqueda
integer width = 2734
integer height = 1752
string title = "Busca Sub Familia"
end type
global w_busc_ctlcalsubfamilia w_busc_ctlcalsubfamilia

on w_busc_ctlcalsubfamilia.create
call super::create
end on

on w_busc_ctlcalsubfamilia.destroy
call super::destroy
end on

event ue_asignacion;CloseWithReturn(This, istr_mant)
end event

event open;istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.retrieve(Long(istr_mant.Argumento[3]))

dw_1.SetRowFocusIndicator(Hand!)

PostEvent("ue_ordenamiento")
/* En ventana heredada se debe setear variables como no selecionado */

is_ordena = 'Código Sub Familia:ccsf_codigo'
/* Esta Variable se utiliza para asignar los Conceptos por los cuales se puede
	realizar el ordenamiento de la dw_1.*/

This.Icon											=	Gstr_apl.Icono

If gs_Ambiente <> "Windows" Then dw_1.Object.DataWindow.Retrieve.AsNeeded	=	"Yes"

istr_mant.Argumento[4] = ''
istr_mant.Argumento[5] = ''

end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_ctlcalsubfamilia
integer x = 2391
integer y = 1008
end type

event pb_insertar::clicked;call super::clicked;CloseWithReturn(parent, istr_mant)
end event

type dw_1 from w_busqueda`dw_1 within w_busc_ctlcalsubfamilia
string dataobject = "dw_busc_ctlcalsubfamilia"
boolean hscrollbar = false
end type

event dw_1::doubleclicked;call super::doubleclicked;IF row < 1 THEN RETURN
istr_mant.Argumento[4] = ''
istr_mant.Argumento[5] = ''
istr_mant.Argumento[4] =  string(dw_1.Object.ccsf_codigo[row])
istr_mant.Argumento[5] =  dw_1.Object.ccsf_descrip[row]


end event

event dw_1::clicked;call super::clicked;IF row < 1 THEN RETURN
istr_mant.Argumento[4] = ''
istr_mant.Argumento[5] = ''
istr_mant.Argumento[4] =  string(dw_1.Object.ccsf_codigo[row])
istr_mant.Argumento[5] =  dw_1.Object.ccsf_descrip[row]
end event

type pb_salir from w_busqueda`pb_salir within w_busc_ctlcalsubfamilia
integer x = 2386
integer y = 1312
end type

event pb_salir::clicked;CloseWithReturn(parent, istr_mant)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_ctlcalsubfamilia
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

