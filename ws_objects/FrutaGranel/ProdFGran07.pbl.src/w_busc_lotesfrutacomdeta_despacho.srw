$PBExportHeader$w_busc_lotesfrutacomdeta_despacho.srw
$PBExportComments$Ventana de Busqueda de Lotes Fruta Comercial.
forward
global type w_busc_lotesfrutacomdeta_despacho from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_especie from datawindow within tabpage_1
end type
type dw_planta from datawindow within tabpage_1
end type
type dw_camara from datawindow within tabpage_1
end type
type st_4 from statictext within tabpage_1
end type
type st_3 from st_argum2 within tabpage_3
end type
type sle_argumento3 from sle_argumento2 within tabpage_3
end type
end forward

global type w_busc_lotesfrutacomdeta_despacho from w_busqueda
integer x = 123
integer y = 304
integer width = 2930
string title = "Búsqueda de Lotes Fruta Comercial"
end type
global w_busc_lotesfrutacomdeta_despacho w_busc_lotesfrutacomdeta_despacho

type variables
Datawindowchild  idwc_planta, idwc_camara, idwc_especie

end variables

on w_busc_lotesfrutacomdeta_despacho.create
int iCurrent
call super::create
end on

on w_busc_lotesfrutacomdeta_despacho.destroy
call super::destroy
end on

event open;call super::open;istr_busq	=	Message.PowerObjectParm

istr_busq.Argum[17]	=	""

is_ordena = 'Número Lote:lofc_lotefc,Variedad:vari_nombre:Productor:prod_nombre,' + &
				'Tipo Frio:frio_tipofr,Periodo Frio:pefr_codigo'

tab_1.tabpage_1.dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

tab_1.tabpage_1.dw_camara.GetChild("cama_codigo", idwc_camara)

tab_1.tabpage_1.dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SqlCa)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

idwc_camara.SetTransObject(SqlCa)
idwc_Camara.SetSort("cama_nombre A")
idwc_Camara.Sort()
idwc_camara.InsertRow(0)

tab_1.tabpage_1.dw_planta.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_camara.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_especie.SetTransObject(SQLCA)

tab_1.tabpage_1.dw_planta.InsertRow(0)
tab_1.tabpage_1.dw_camara.InsertRow(0)
tab_1.tabpage_1.dw_especie.InsertRow(0)

IF istr_busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_planta.SetItem(1,"plde_codigo",Integer(istr_busq.Argum[1]))
	idwc_camara.Retrieve(Integer(istr_busq.Argum[1]))
	tab_1.tabpage_1.dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192,192,192)
	tab_1.tabpage_1.dw_planta.Enabled	=	False
END IF

IF istr_busq.Argum[2] <> "" THEN
	tab_1.tabpage_1.dw_camara.SetItem(1,"cama_codigo",Integer(istr_busq.Argum[2]))
	tab_1.tabpage_1.dw_camara.Object.cama_codigo.BackGround.Color	=	RGB(192,192,192)
	tab_1.tabpage_1.dw_camara.Enabled	=	False
END IF

IF istr_busq.Argum[3] <> "" THEN
	tab_1.tabpage_1.dw_especie.SetItem(1,"espe_codigo",Integer(istr_busq.Argum[3]))
	tab_1.tabpage_1.dw_especie.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
	tab_1.tabpage_1.dw_especie.Enabled	=	False
	tab_1.tabpage_1.pb_filtrar.PostEvent(Clicked!)
END IF

tab_1.tabpage_1.pb_filtrar.SetFocus()
end event

event ue_asignacion();istr_Busq.Argum[1]	= String(dw_1.Object.lofc_pltcod[dw_1.GetRow()])
istr_Busq.Argum[2]	= String(dw_1.Object.lofc_espcod[dw_1.GetRow()])
istr_Busq.Argum[3]	= String(dw_1.Object.lofc_lotefc[dw_1.GetRow()])
istr_Busq.Argum[4]	= String(dw_1.Object.lfcd_secuen[dw_1.GetRow()])
istr_Busq.Argum[5]	= String(dw_1.Object.vari_codigo[dw_1.GetRow()])
istr_Busq.Argum[6]	= dw_1.Object.vari_nombre[dw_1.GetRow()]
istr_Busq.Argum[7]	= String(dw_1.Object.prod_codigo[dw_1.GetRow()])
istr_Busq.Argum[8]	= dw_1.Object.prod_nombre[dw_1.GetRow()]
istr_Busq.Argum[9]	= String(dw_1.Object.frio_tipofr[dw_1.GetRow()])
istr_Busq.Argum[10]	= String(dw_1.Object.pefr_codigo[dw_1.GetRow()])
istr_Busq.Argum[11]	= String(dw_1.Object.lfcd_tipdoc[dw_1.GetRow()])
istr_Busq.Argum[12]	= String(dw_1.Object.lfcd_docrel[dw_1.GetRow()])
istr_Busq.Argum[13]	= String(dw_1.Object.caex_canbul[dw_1.GetRow()])
istr_Busq.Argum[14]	= String(dw_1.Object.cama_codigo[dw_1.GetRow()])
istr_Busq.Argum[15]	= String(dw_1.Object.enva_tipoen[dw_1.GetRow()])
istr_Busq.Argum[16]	= String(dw_1.Object.enva_codigo[dw_1.GetRow()])
istr_Busq.Argum[17]	= String(dw_1.Object.enva_nombre[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

event resize;call super::resize;Long		maximo

IF tab_1.Width > dw_1.Width THEN
	maximo = tab_1.Width
ELSE
	maximo = dw_1.Width
END IF

This.Width		= maximo + 540


pb_salir.x		= This.WorkSpaceWidth() - 292
pb_salir.y		= This.WorkSpaceHeight() - 264
pb_insertar.x	= This.WorkSpaceWidth() - 292
pb_insertar.y	= pb_salir.y - 304
tab_1.x			= 78
tab_1.y			= 69
dw_1.x			= 78
dw_1.y			= tab_1.Height + 136

end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_lotesfrutacomdeta_despacho
boolean visible = false
integer x = 2601
integer y = 1068
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_lotesfrutacomdeta_despacho
integer x = 82
integer y = 740
integer width = 2368
integer taborder = 60
string dataobject = "dw_mues_lotesfrutacomdeta_despacho"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_lotesfrutacomdeta_despacho
integer x = 2597
end type

event pb_salir::clicked;
istr_busq.argum[3] = ""
istr_busq.argum[4] = ""
istr_busq.argum[5] = ""
istr_busq.argum[6] = ""

CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_lotesfrutacomdeta_despacho
integer x = 105
integer y = 80
integer width = 2171
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 2135
string text = "Filtros              "
st_1 st_1
st_2 st_2
dw_especie dw_especie
dw_planta dw_planta
dw_camara dw_camara
st_4 st_4
end type

on tabpage_1.create
this.st_1=create st_1
this.st_2=create st_2
this.dw_especie=create dw_especie
this.dw_planta=create dw_planta
this.dw_camara=create dw_camara
this.st_4=create st_4
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_especie
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.dw_camara
this.Control[iCurrent+6]=this.st_4
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_especie)
destroy(this.dw_planta)
destroy(this.dw_camara)
destroy(this.st_4)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1577
integer y = 312
integer taborder = 40
boolean default = false
end type

event pb_filtrar::clicked;dw_1.SetRedraw(False)			  

IF (istr_busq.argum[1] <> "") and (istr_busq.argum[2]<>"") and (istr_busq.argum[3]<>"") THEN

	IF dw_1.Retrieve(Integer(istr_busq.argum[1]), Integer(istr_busq.argum[2]), &
					  Integer(istr_busq.argum[3])) > 0 THEN
		
      IF istr_busq.argum[4]<>""  THEN
		   IF istr_busq.argum[4]="25" THEN
				dw_1.SetFilter("lfcd_tipool = 1")
				dw_1.Filter()	
			ELSE	
				dw_1.SetFilter("prod_codigo =" + istr_busq.argum[4])
				dw_1.Filter()
			END IF	
		END IF
		
		IF dw_1.RowCount() = 0 THEN
			MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
		ELSE	
			dw_1.SetFocus()
			dw_1.SelectRow(1,True)
		END IF
	ELSE
		MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	END IF
END IF	

dw_1.SetRedraw(TRUE)
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2135
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1760
integer y = 312
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 695
integer width = 914
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer width = 658
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2135
string text = "Búsqueda          "
st_3 st_3
sle_argumento3 sle_argumento3
end type

on tabpage_3.create
this.st_3=create st_3
this.sle_argumento3=create sle_argumento3
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.sle_argumento3
end on

on tabpage_3.destroy
call super::destroy
destroy(this.st_3)
destroy(this.sle_argumento3)
end on

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 645
long backcolor = 12632256
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(192,192,192)
sle_argumento3.TabOrder		= 0
es_numero						= False
is_busca							= "vari_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 169
string text = "Variedad"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 645
integer width = 306
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(192,192,192)
sle_argumento3.TabOrder		= 0
es_numero						= True
is_busca							= "lofc_lotefc"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 169
integer width = 389
string text = "Número Lote"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1760
integer y = 300
end type

type st_1 from statictext within tabpage_1
integer x = 110
integer y = 104
integer width = 288
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_1
integer x = 110
integer y = 228
integer width = 256
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Camara"
boolean focusrectangle = false
end type

type dw_especie from datawindow within tabpage_1
integer x = 430
integer y = 308
integer width = 873
integer height = 96
integer taborder = 30
boolean bringtotop = true
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "espe_codigo"
		istr_busq.Argum[3]	=	data
END CHOOSE
end event

type dw_planta from datawindow within tabpage_1
integer x = 430
integer y = 92
integer width = 873
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "plde_codigo"
		istr_busq.Argum[1]	=	data
		tab_1.tabpage_1.dw_camara.GetChild("cama_codigo", idwc_camara)
		tab_1.tabpage_1.dw_camara.SetTransObject(SQLCA)
		idwc_camara.Retrieve(Integer(istr_busq.Argum[1]))
		idwc_Camara.SetSort("cama_nombre A")
		idwc_Camara.Sort()

END CHOOSE
end event

type dw_camara from datawindow within tabpage_1
integer x = 430
integer y = 200
integer width = 873
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_camarasfrigo"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "cama_codigo"
		istr_busq.Argum[2]	=	data

END CHOOSE
end event

type st_4 from statictext within tabpage_1
integer x = 110
integer y = 336
integer width = 256
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Especie"
boolean focusrectangle = false
end type

type st_3 from st_argum2 within tabpage_3
integer y = 328
boolean bringtotop = true
string text = "Productor"
end type

type sle_argumento3 from sle_argumento2 within tabpage_3
integer y = 320
integer taborder = 25
boolean bringtotop = true
end type

event getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= False
is_busca							= "vari_nombre"
end event

