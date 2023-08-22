$PBExportHeader$w_busc_ctlcalplacuaninspuvaenc.srw
forward
global type w_busc_ctlcalplacuaninspuvaenc from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_plantas from datawindow within tabpage_1
end type
type cbx_planta from checkbox within tabpage_1
end type
end forward

global type w_busc_ctlcalplacuaninspuvaenc from w_busqueda
integer x = 78
integer y = 176
integer width = 3250
integer height = 1832
string title = "Verificación de Producto Terminado"
boolean resizable = false
long backcolor = 33543637
end type
global w_busc_ctlcalplacuaninspuvaenc w_busc_ctlcalplacuaninspuvaenc

type variables
DataWindowChild	idwc_planta,idwc_prod,idwc_espe

Integer	ii_fila
Integer  ii_SwOrden[]	=	{0, 0, 0}
end variables

event open;Long		ll_fila = 1

x	=	30
y	=	280

istr_busq = Message.PowerObjectParm

tab_1.tabpage_1.dw_plantas.GetChild("plde_codigo", idwc_planta)

idwc_planta.SetTransObject(sqlca)

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

idwc_planta.Retrieve(1)

tab_1.tabpage_1.dw_plantas.SetTransObject(sqlca)
tab_1.tabpage_1.dw_plantas.InsertRow(0)

is_ordena = 'Fecha Inspección:ccpe_fechin,Nº. Lote:cclo_numero,Nº. Planilla:ccpe_numero,Productor:prod_codigo'

TriggerEvent("ue_ordenamiento")
istr_busq.argum[2]	= '9999'

istr_busq.argum[30]	= '1'			//Se Utiliza Como Sw en la maed.

ii_fila = 1

tab_1.tabpage_1.pb_Filtrar.PostEvent(Clicked!)
end event

on w_busc_ctlcalplacuaninspuvaenc.create
int iCurrent
call super::create
end on

on w_busc_ctlcalplacuaninspuvaenc.destroy
call super::destroy
end on

event ue_asignacion;Integer	li_planta, li_lote, li_especie, li_variedad,  li_packing, li_zona
String	ls_embalaje, ls_calibre
Date		ld_fecemb, ld_fechin
Long     ll_productor

li_planta	=	dw_1.Object.plde_codigo[ii_fila]
li_lote		=	dw_1.Object.cclo_numero[ii_fila]

istr_busq.argum[1]	=	String(dw_1.Object.cclo_numero[ii_fila])
istr_busq.argum[2]	=	String(dw_1.Object.ccpe_numero[ii_fila])
istr_busq.argum[3]	=	String(dw_1.Object.zona_codigo[ii_fila])
istr_busq.argum[4]	=	String(dw_1.Object.plde_codigo[ii_fila])
istr_busq.argum[7]	=	String(gi_CodExport)
istr_busq.Argum[14]	=	String(dw_1.Object.ccpe_estado[ii_fila])


SELECT	prod_codigo, espe_codigo, vari_codigo, 
			emba_codigo, vaca_calibr, plde_codpak, cclo_fecemb
	INTO	:ll_productor,:li_especie,:li_variedad,
			:ls_embalaje,:ls_calibre,:li_packing,:ld_fecemb
	FROM	dba.CTLCALLOTES
	WHERE	 plde_codigo =	:li_planta
	AND    cclo_numero = :li_lote;
				
istr_busq.argum[8]	=	String(ll_productor)
istr_busq.argum[9]	=	String(li_especie)
istr_busq.argum[10]	=	String(li_variedad)
istr_busq.argum[17]	=	ls_embalaje
istr_busq.argum[18]	=	String(ld_fecemb)
istr_busq.argum[20]	=	String(li_packing)
istr_busq.argum[21]	=	ls_calibre

SELECT	zona_codigo, ccpe_fechin
	INTO	:li_zona, :ld_fechin
	FROM	dba.CTLCALPLACUANINSPUVAENC
	WHERE	plde_codigo =	:li_planta
	AND   cclo_numero = :li_lote;

istr_busq.argum[3]	= String(li_zona)
istr_busq.argum[5]	= String(ld_fechin)

CloseWithReturn(This,istr_busq)
end event

event resize;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_ctlcalplacuaninspuvaenc
boolean visible = false
integer x = 2898
integer y = 1128
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_ctlcalplacuaninspuvaenc
integer y = 760
integer width = 2816
integer height = 900
integer taborder = 60
string dataobject = "dw_mues_ctlcalplacuaninspuvaencdefi"
end type

event dw_1::doubleclicked;call super::doubleclicked;IF row > 0 THEN ii_fila	=	row
istr_busq.argum[30]	= '0'
end event

event dw_1::clicked;String	ls_Tecla, ls_Columna, ls_Orden[] = {" A", " D"}, &
			ls_Columnas[]	=	{"ccpe_numero", "cclo_numero", "ccpe_fechin"}, &
			ls_Ordenam		=	"&planilla01&lote02&fecha03"
Long		ll_Fila, ll_Numero
Integer	li_Posicion

IF dw_1.RowCount() > 0 THEN
	IF Row > 0 THEN
		ii_fila	=	Row
		This.SelectRow(0,False)
		This.SelectRow(Row,True)
		This.SetRow(Row)
	ELSE
		ll_Numero	=	dw_1.Object.ccpe_numero[ii_fila]
		ls_Columna	=	dwo.Name
		li_Posicion	=	Pos(ls_Ordenam, "&" + ls_Columna) + 1
		li_Posicion	=	Integer(Mid(ls_Ordenam, li_Posicion + Len(ls_Columna), 2))
	
		IF li_Posicion > 0 THEN
		 dw_1.SetSort(ls_Columnas[li_Posicion] + ls_Orden[ii_SwOrden[li_Posicion] + 1])
		 dw_1.Sort()
		 ii_SwOrden[li_Posicion]	=	Mod(ii_SwOrden[li_Posicion] + 1, 2)
		 ii_fila		=	dw_1.Find("ccpe_numero = " + String(ll_Numero), 1, dw_1.RowCount())
		END IF
	END IF
END IF
end event

type pb_salir from w_busqueda`pb_salir within w_busc_ctlcalplacuaninspuvaenc
integer x = 2981
integer y = 1404
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_ctlcalplacuaninspuvaenc
integer x = 69
integer y = 96
integer width = 2816
integer height = 628
long backcolor = 33543637
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 2779
integer height = 500
long backcolor = 33543637
string text = "Filtros                         "
st_1 st_1
dw_plantas dw_plantas
cbx_planta cbx_planta
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_plantas=create dw_plantas
this.cbx_planta=create cbx_planta
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_plantas
this.Control[iCurrent+3]=this.cbx_planta
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_plantas)
destroy(this.cbx_planta)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2560
integer y = 312
end type

event pb_filtrar::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1]), Integer(istr_busq.argum[2])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	IF tab_1.tabpage_1.dw_plantas.Enabled THEN
		tab_1.tabpage_1.dw_plantas.SetFocus()
	END IF
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2779
integer height = 500
long backcolor = 33543637
string text = "Ordenamiento              "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2560
integer y = 312
integer taborder = 20
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer taborder = 30
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer taborder = 10
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
boolean visible = false
integer width = 2779
integer height = 500
boolean enabled = false
long backcolor = 33543637
string text = "Búsqueda                      "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 704
integer y = 252
integer width = 411
long backcolor = 30586022
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))

This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "cclo_numero"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 133
integer y = 260
long backcolor = 33543637
string text = "Nº. Planilla"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 704
integer y = 132
integer width = 411
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "prod_codigo"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 133
integer y = 140
integer width = 521
long backcolor = 33543637
string text = "Productor"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2560
integer y = 312
end type

type st_1 from statictext within tabpage_1
integer x = 302
integer y = 200
integer width = 274
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Planta "
boolean focusrectangle = false
end type

type dw_plantas from datawindow within tabpage_1
integer x = 626
integer y = 184
integer width = 960
integer height = 96
integer taborder = 11
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.argum[2] = data
end event

type cbx_planta from checkbox within tabpage_1
integer x = 626
integer y = 92
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;String	ls_null

SetNull(ls_null)

IF This.Checked THEN
	dw_plantas.Enabled	=	False
   dw_plantas.SetItem(1,"plde_codigo", ls_null)
	istr_busq.argum[2]	= '9999'
ELSE
	dw_plantas.Enabled	=	True
END IF

end event

