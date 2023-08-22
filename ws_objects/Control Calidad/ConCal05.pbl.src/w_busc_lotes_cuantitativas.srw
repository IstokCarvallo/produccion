$PBExportHeader$w_busc_lotes_cuantitativas.srw
$PBExportComments$Búsqueda de Lotes Con Iguales Características
forward
global type w_busc_lotes_cuantitativas from w_busqueda
end type
end forward

global type w_busc_lotes_cuantitativas from w_busqueda
integer width = 3593
string title = "Búsqueda de Lotes"
end type
global w_busc_lotes_cuantitativas w_busc_lotes_cuantitativas

type variables
integer ii_fila

datawindowchild idwc_variedades, idwc_especies, idwc_productor, idwc_packing
end variables

on w_busc_lotes_cuantitativas.create
call super::create
end on

on w_busc_lotes_cuantitativas.destroy
call super::destroy
end on

event open;Integer li_cliente, li_especie
String  busca

dw_1.SetTransObject(sqlca)

istr_busq = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

PostEvent("ue_ordenamiento")

is_ordena = 'Productor:prod_codigo,Especie:espe_codigo,Lote:cclo_numero'
busca		 = "cclo_numero"

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
idwc_productor.Retrieve(-1)

//packing//
dw_1.GetChild("plde_codpak", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2,0)

IF IsNull(istr_busq.Argum[13]) THEN istr_busq.Argum[13] = '*'
IF IsNull(istr_busq.Argum[14]) THEN istr_busq.Argum[14] = '1900/01/01'

IF dw_1.Retrieve(Integer(istr_busq.Argum[3]),Integer(istr_busq.Argum[1]),Long(istr_busq.Argum[2]),&
                 Integer(istr_busq.Argum[12]),istr_busq.Argum[13],Date(istr_busq.Argum[14])) < 1 THEN
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	istr_busq.argum[2]=""
	CloseWithReturn(This,istr_busq)
END IF
end event

event ue_asignacion;istr_busq.argum[2]	=	String(dw_1.object.prod_codigo[ii_fila])
istr_busq.argum[3]	=	String(dw_1.object.espe_codigo[ii_fila])
istr_busq.argum[4]	=	String(dw_1.object.vari_codigo[ii_Fila])
istr_busq.argum[5]	=	dw_1.object.emba_codigo[ii_Fila]
istr_busq.argum[6]	=	String(dw_1.object.vaca_calibr[ii_Fila])
istr_busq.argum[7]	=	String(dw_1.object.plde_codpak[ii_Fila])
istr_busq.argum[8]	=	String(dw_1.object.cclo_fecemb[ii_Fila])
istr_busq.argum[9]	=	String(dw_1.object.cclo_tamlot[ii_Fila])
istr_busq.argum[10]	=	String(dw_1.object.plde_codigo[ii_Fila])
istr_busq.argum[11]	=	String(dw_1.object.cclo_numero[ii_Fila])


CloseWithReturn(This,istr_busq)
end event

event resize;Long		maximo

IF tab_1.Width > dw_1.Width THEN
	maximo = tab_1.Width
ELSE
	maximo = dw_1.Width
END IF

This.x			= 40
This.y			= 285	
This.Width		= maximo + 540
//gb_1.x 			= This.WorkSpaceWidth() - 351
//gb_1.y			= This.WorkSpaceHeight() - 348
//gb_2.x			= gb_1.x
//gb_2.y			= gb_1.y - 304
pb_salir.x		= This.WorkSpaceWidth() - 292
pb_salir.y		= This.WorkSpaceHeight() - 264
pb_insertar.x	= This.WorkSpaceWidth() - 292
pb_insertar.y	= pb_salir.y - 304
tab_1.x			= 78
tab_1.y			= 69
dw_1.x			= 78
dw_1.y			= tab_1.Height + 136

end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_lotes_cuantitativas
boolean visible = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_lotes_cuantitativas
integer y = 696
integer width = 3077
string dataobject = "dw_mues_lotes_cuantitativas1"
boolean minbox = true
boolean hsplitscroll = true
end type

event dw_1::doubleclicked;call super::doubleclicked;ii_fila	=	row
istr_busq.argum[30]	= '0'
end event

type pb_salir from w_busqueda`pb_salir within w_busc_lotes_cuantitativas
integer x = 3269
end type

event pb_salir::clicked;istr_busq.argum[2] = ""
istr_busq.argum[30] = ""
CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_lotes_cuantitativas
integer x = 82
integer width = 3072
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
integer width = 3035
boolean enabled = false
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
boolean visible = false
end type

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 3035
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 841
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 3035
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
boolean visible = false
integer x = 864
integer width = 649
end type

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
boolean visible = false
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 864
integer width = 297
end type

event sle_argumento1::modified;call super::modified;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0

es_numero						= True
is_busca							= "cclo_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer width = 389
string text = "Número Lote"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
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

