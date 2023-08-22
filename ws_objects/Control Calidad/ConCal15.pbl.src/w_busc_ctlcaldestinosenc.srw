$PBExportHeader$w_busc_ctlcaldestinosenc.srw
forward
global type w_busc_ctlcaldestinosenc from w_busqueda
end type
end forward

global type w_busc_ctlcaldestinosenc from w_busqueda
integer width = 2638
string title = "Búsqueda de Planillas Destino"
end type
global w_busc_ctlcaldestinosenc w_busc_ctlcaldestinosenc

type variables
integer ii_fila
datawindowchild idwc_variedades, idwc_especies, idwc_productor
end variables

on w_busc_ctlcaldestinosenc.create
call super::create
end on

on w_busc_ctlcaldestinosenc.destroy
call super::destroy
end on

event open;Integer li_cliente, li_especie

dw_1.SetTransObject(sqlca)

istr_busq = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

PostEvent("ue_ordenamiento")

is_ordena = 'Nave:nave_codigo,Productor:prod_codigo,Especie:espe_codigo'

This.Icon	=	Gstr_apl.Icono

//variedad//
dw_1.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
idwc_variedades.Retrieve(gi_codexport,gi_codespecie)

//especie//
dw_1.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(sqlca)
idwc_especies.Retrieve(gi_codexport)

//productor//
dw_1.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(gi_codexport)

dw_1.retrieve()
end event

event ue_asignacion();
istr_busq.argum[15]	=	dw_1.object.nave_tipotr[dw_1.GetRow()]
istr_busq.argum[1]	=	String(dw_1.object.nave_codigo[dw_1.GetRow()])
istr_busq.argum[2]	=	dw_1.object.ccde_bodega[dw_1.GetRow()]
istr_busq.argum[3]	=	String(dw_1.object.puer_codigo[dw_1.GetRow()])
istr_busq.argum[4]	=	dw_1.object.ccde_lugins[dw_1.GetRow()]
istr_busq.argum[5]	=	String(dw_1.object.ccde_fecarr[dw_1.GetRow()])
istr_busq.argum[6]	=	String(dw_1.object.ccde_fecins[dw_1.GetRow()])
istr_busq.argum[7]	=	String(dw_1.object.ccde_fecfum[dw_1.GetRow()])
istr_busq.argum[8]	=	String(dw_1.object.ccde_temmax[dw_1.GetRow()])
istr_busq.argum[9]	=	String(dw_1.object.ccde_temmin[dw_1.GetRow()])
istr_busq.argum[10]	=	String(dw_1.object.ccde_temfre[dw_1.GetRow()])
istr_busq.argum[11]	=	String(dw_1.object.vari_codigo[dw_1.GetRow()])
istr_busq.argum[12]	=	String(dw_1.object.ccin_codigo[dw_1.GetRow()])
istr_busq.argum[13]	=	String(dw_1.object.reci_codigo[dw_1.GetRow()])
istr_busq.argum[14]	=	String(dw_1.object.prod_codigo[dw_1.GetRow()])
istr_busq.argum[16]	=	String(dw_1.object.etiq_codigo[dw_1.GetRow()])
istr_busq.argum[17]	=	dw_1.object.emba_codigo[dw_1.GetRow()]
istr_busq.argum[23]	=	dw_1.object.vaca_calibr[dw_1.GetRow()]
istr_busq.argum[18]	=	String(dw_1.object.ccde_fecemb[dw_1.GetRow()])
istr_busq.argum[19]	=	dw_1.object.ccde_reslot[dw_1.GetRow()]
istr_busq.argum[20]	=	String(dw_1.object.ccde_cauobj[dw_1.GetRow()])
istr_busq.argum[21]	=	String(dw_1.object.ccde_causa2[dw_1.GetRow()])
istr_busq.argum[22]	=	String(dw_1.object.ccde_causa3[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_ctlcaldestinosenc
boolean visible = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_ctlcaldestinosenc
integer x = 105
integer y = 696
string dataobject = "dw_mues_ctlcaldestinosenc"
boolean minbox = true
boolean border = false
boolean hsplitscroll = true
end type

event dw_1::doubleclicked;call super::doubleclicked;ii_fila	=	row
istr_busq.argum[30]	= '0'
end event

type pb_salir from w_busqueda`pb_salir within w_busc_ctlcaldestinosenc
end type

event pb_salir::clicked;istr_busq.argum[30] = ""
CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_ctlcaldestinosenc
integer x = 82
integer selectedtab = 2
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
boolean visible = false
boolean enabled = false
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
boolean visible = false
end type

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 841
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

