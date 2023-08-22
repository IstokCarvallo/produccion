$PBExportHeader$w_busc_lotes_cualitativa.srw
$PBExportComments$Muestra lista de lotes con misma identificación pero con distinto número de lote, para la planilla cuantitativa.
forward
global type w_busc_lotes_cualitativa from w_busqueda
end type
end forward

global type w_busc_lotes_cualitativa from w_busqueda
integer width = 2638
string title = "Búsqueda de Planillas Cualitativas"
end type
global w_busc_lotes_cualitativa w_busc_lotes_cualitativa

type variables
integer ii_fila
datawindowchild idwc_variedades, idwc_especies, idwc_productor
end variables

on w_busc_lotes_cualitativa.create
call super::create
end on

on w_busc_lotes_cualitativa.destroy
call super::destroy
end on

event open;//*******************************************************************//
//**************************Argumentos Busq**************************//
/*
istr_busq.argum[1]	=  Código Cliente
istr_busq.argum[2]	=	Código Planta
istr_busq.argum[3]	=	Código Productor
istr_busq.argum[4]	=	Código Especie
istr_busq.argum[5]	=	Código Variedad
istr_busq.argum[6]	=	Código Embalaje
istr_busq.argum[7]	=	Calibre
istr_busq.argum[8]	=	Código Packing
istr_busq.argum[9]	=	Fecha Embalaje
istr_busq.argum[10]	=	Tamaño
istr_busq.argum[11]	=	Número Lote
istr_busq.Argum[12]	=	Fecha de Aprobación
istr_busq.Argum[13]	=	Estado Lote
*/
//*******************************************************************//
//**************************Argumentos Busq**************************//

Integer li_cliente, li_especie

dw_1.SetTransObject(sqlca)

istr_busq.argum[1]	=  ''
istr_busq.argum[2]	=	''
istr_busq.argum[3]	=	''
istr_busq.argum[4]	=	''
istr_busq.argum[5]	=	''
istr_busq.argum[6]	=	''
istr_busq.argum[7]	=	''
istr_busq.argum[8]	=	''
istr_busq.argum[9]	=	''
istr_busq.argum[10]	=	''
istr_busq.argum[11]	=	''
istr_busq.Argum[12]	=	''
istr_busq.Argum[13]	=	''

istr_busq = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

PostEvent("ue_ordenamiento")

is_ordena = 'Productor:prod_codigo,Especie:espe_codigo'

This.Icon	=	Gstr_apl.Icono

//variedad//
dw_1.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
idwc_variedades.Retrieve(gi_codespecie)

//especie//
dw_1.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(sqlca)
idwc_especies.Retrieve()

//productor//
dw_1.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve()

dw_1.Retrieve(Integer(istr_Busq.Argum[1]),81,Long(istr_Busq.Argum[3]))

IF dw_1.RowCount()	=	0	THEN
	MessageBox("Atención","No hay Planillas con Lotes Objetados")
END IF


end event

event ue_asignacion();
istr_busq.argum[1]	=  String(dw_1.Object.clie_codigo[dw_1.Getrow()])
istr_busq.argum[2]	=	String(dw_1.object.plde_codigo[dw_1.Getrow()])
istr_busq.argum[3]	=	String(dw_1.object.prod_codigo[dw_1.Getrow()])
istr_busq.argum[4]	=	String(dw_1.object.espe_codigo[dw_1.Getrow()])
istr_busq.argum[5]	=	String(dw_1.Object.vari_codigo[dw_1.Getrow()])
istr_busq.argum[6]	=	dw_1.object.emba_codigo[dw_1.Getrow()]
istr_busq.argum[7]	=	dw_1.object.vaca_calibr[dw_1.Getrow()]
istr_busq.argum[8]	=	String(dw_1.object.plde_codpak[dw_1.Getrow()])
istr_busq.argum[9]	=	String(dw_1.object.cclo_fecemb[dw_1.Getrow()])
istr_busq.argum[10]	=	String(dw_1.object.cclo_tamlot[dw_1.Getrow()])
istr_busq.argum[11]	=	String(dw_1.object.cclo_numero[dw_1.Getrow()])
istr_busq.Argum[12]	=	String(dw_1.object.cclo_fecapr[dw_1.Getrow()])
istr_busq.Argum[13]	=	String(dw_1.object.cclo_estado[dw_1.Getrow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_lotes_cualitativa
boolean visible = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_lotes_cualitativa
integer y = 696
string dataobject = "dw_mues_lotes_cualitativa"
boolean minbox = true
boolean hsplitscroll = true
end type

event dw_1::doubleclicked;call super::doubleclicked;ii_fila	=	row
istr_busq.argum[30]	= '0'
end event

type pb_salir from w_busqueda`pb_salir within w_busc_lotes_cualitativa
end type

event pb_salir::clicked;istr_busq.argum[30] = ""
CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_lotes_cualitativa
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

