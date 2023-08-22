$PBExportHeader$w_busc_lotesdisponibles_distcalib.srw
forward
global type w_busc_lotesdisponibles_distcalib from w_busqueda
end type
end forward

global type w_busc_lotesdisponibles_distcalib from w_busqueda
integer width = 3209
end type
global w_busc_lotesdisponibles_distcalib w_busc_lotesdisponibles_distcalib

event open;call super::open;is_ordena = 'Predio:prbr_codpre:Cuartel:prcc_codigo'

istr_busq	=	Message.PowerObjectParm

tab_1.TabPage_1.Visible	=	False

istr_busq.Argum[5]	=	''
istr_busq.Argum[6]	=	''
istr_busq.Argum[7]	=	''
istr_busq.Argum[8]	=	''

IF dw_1.retrieve(Integer(istr_busq.argum[1]), Long(istr_busq.argum[2]), &
					  Integer(istr_busq.argum[3]), Long(istr_busq.argum[4])) < 1 THEN
	MessageBox("No Existen Lotes", "Todos los lotes asignados a la orden de proceso ya están asignados")
	CloseWithReturn(This, istr_busq)
END IF
end event

on w_busc_lotesdisponibles_distcalib.create
call super::create
end on

on w_busc_lotesdisponibles_distcalib.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.Argum[5]	=	String(dw_1.Object.lote_pltcod[dw_1.GetRow()])
istr_busq.Argum[6]	=	String(dw_1.Object.lote_espcod[dw_1.GetRow()])
istr_busq.Argum[7]	=	String(dw_1.Object.lote_codigo[dw_1.GetRow()])
istr_busq.Argum[8]	=	String(dw_1.Object.pprc_calini[dw_1.GetRow()])

CloseWithReturn(This, istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_lotesdisponibles_distcalib
integer x = 2866
end type

type dw_1 from w_busqueda`dw_1 within w_busc_lotesdisponibles_distcalib
integer width = 2679
string dataobject = "dw_mues_lotesdisponibles_distcalibre"
end type

event dw_1::doubleclicked;This.SetRow(row)

IF row > 0 THEN
	Parent.PostEvent("ue_asignacion")
END IF
end event

type pb_salir from w_busqueda`pb_salir within w_busc_lotesdisponibles_distcalib
integer x = 2866
end type

type tab_1 from w_busqueda`tab_1 within w_busc_lotesdisponibles_distcalib
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
end type

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
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

