$PBExportHeader$w_info_existencia_productor_calibre.srw
$PBExportComments$Informe de Existencia de Fruta Comercial en Camara por Productor y Calibre.
forward
global type w_info_existencia_productor_calibre from w_para_informes
end type
type st_1 from statictext within w_info_existencia_productor_calibre
end type
type uo_selprod from uo_seleccion_productor within w_info_existencia_productor_calibre
end type
type st_2 from statictext within w_info_existencia_productor_calibre
end type
type st_3 from statictext within w_info_existencia_productor_calibre
end type
type st_4 from statictext within w_info_existencia_productor_calibre
end type
type st_5 from statictext within w_info_existencia_productor_calibre
end type
type uo_selvari from uo_seleccion_variedad within w_info_existencia_productor_calibre
end type
type uo_selplta from uo_seleccion_plantas within w_info_existencia_productor_calibre
end type
type uo_selcama from uo_seleccion_camara within w_info_existencia_productor_calibre
end type
type uo_selfrio from uo_seleccion_tipofrio within w_info_existencia_productor_calibre
end type
type uo_selcate from uo_seleccion_categoria within w_info_existencia_productor_calibre
end type
type st_6 from statictext within w_info_existencia_productor_calibre
end type
type st_7 from statictext within w_info_existencia_productor_calibre
end type
type st_8 from statictext within w_info_existencia_productor_calibre
end type
type st_9 from statictext within w_info_existencia_productor_calibre
end type
type cbx_conslote from checkbox within w_info_existencia_productor_calibre
end type
type rb_detabultos from radiobutton within w_info_existencia_productor_calibre
end type
type rb_detakilos from radiobutton within w_info_existencia_productor_calibre
end type
type rb_proespvar from radiobutton within w_info_existencia_productor_calibre
end type
type rb_espprovar from radiobutton within w_info_existencia_productor_calibre
end type
type rb_espvarpro from radiobutton within w_info_existencia_productor_calibre
end type
type gb_detalle from groupbox within w_info_existencia_productor_calibre
end type
type gb_ordena from groupbox within w_info_existencia_productor_calibre
end type
type st_10 from statictext within w_info_existencia_productor_calibre
end type
type st_12 from statictext within w_info_existencia_productor_calibre
end type
type st_14 from statictext within w_info_existencia_productor_calibre
end type
type rb_retiro from radiobutton within w_info_existencia_productor_calibre
end type
type gb_3 from groupbox within w_info_existencia_productor_calibre
end type
type st_13 from statictext within w_info_existencia_productor_calibre
end type
type rb_venta from radiobutton within w_info_existencia_productor_calibre
end type
type st_16 from statictext within w_info_existencia_productor_calibre
end type
type uo_selespe from uo_seleccion_especie within w_info_existencia_productor_calibre
end type
type st_11 from statictext within w_info_existencia_productor_calibre
end type
type uo_selexpo from uo_seleccion_clientesprod within w_info_existencia_productor_calibre
end type
type st_15 from statictext within w_info_existencia_productor_calibre
end type
end forward

global type w_info_existencia_productor_calibre from w_para_informes
integer x = 0
integer y = 0
integer width = 3611
integer height = 1888
st_1 st_1
uo_selprod uo_selprod
st_2 st_2
st_3 st_3
st_4 st_4
st_5 st_5
uo_selvari uo_selvari
uo_selplta uo_selplta
uo_selcama uo_selcama
uo_selfrio uo_selfrio
uo_selcate uo_selcate
st_6 st_6
st_7 st_7
st_8 st_8
st_9 st_9
cbx_conslote cbx_conslote
rb_detabultos rb_detabultos
rb_detakilos rb_detakilos
rb_proespvar rb_proespvar
rb_espprovar rb_espprovar
rb_espvarpro rb_espvarpro
gb_detalle gb_detalle
gb_ordena gb_ordena
st_10 st_10
st_12 st_12
st_14 st_14
rb_retiro rb_retiro
gb_3 gb_3
st_13 st_13
rb_venta rb_venta
st_16 st_16
uo_selespe uo_selespe
st_11 st_11
uo_selexpo uo_selexpo
st_15 st_15
end type
global w_info_existencia_productor_calibre w_info_existencia_productor_calibre

forward prototypes
public subroutine consolidalote ()
end prototypes

public subroutine consolidalote ();IF uo_SelProd.Codigo = -9 OR &
	uo_SelEspe.Codigo = -9 OR &
	uo_SelVari.Codigo = -9 THEN
	cbx_ConsLote.Checked	=	False
	cbx_ConsLote.Enabled	=	False
ELSE
	cbx_ConsLote.Checked	=	True
	cbx_ConsLote.Enabled	=	True
END IF

RETURN
end subroutine

on w_info_existencia_productor_calibre.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selprod=create uo_selprod
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.st_5=create st_5
this.uo_selvari=create uo_selvari
this.uo_selplta=create uo_selplta
this.uo_selcama=create uo_selcama
this.uo_selfrio=create uo_selfrio
this.uo_selcate=create uo_selcate
this.st_6=create st_6
this.st_7=create st_7
this.st_8=create st_8
this.st_9=create st_9
this.cbx_conslote=create cbx_conslote
this.rb_detabultos=create rb_detabultos
this.rb_detakilos=create rb_detakilos
this.rb_proespvar=create rb_proespvar
this.rb_espprovar=create rb_espprovar
this.rb_espvarpro=create rb_espvarpro
this.gb_detalle=create gb_detalle
this.gb_ordena=create gb_ordena
this.st_10=create st_10
this.st_12=create st_12
this.st_14=create st_14
this.rb_retiro=create rb_retiro
this.gb_3=create gb_3
this.st_13=create st_13
this.rb_venta=create rb_venta
this.st_16=create st_16
this.uo_selespe=create uo_selespe
this.st_11=create st_11
this.uo_selexpo=create uo_selexpo
this.st_15=create st_15
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selprod
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.uo_selvari
this.Control[iCurrent+8]=this.uo_selplta
this.Control[iCurrent+9]=this.uo_selcama
this.Control[iCurrent+10]=this.uo_selfrio
this.Control[iCurrent+11]=this.uo_selcate
this.Control[iCurrent+12]=this.st_6
this.Control[iCurrent+13]=this.st_7
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.st_9
this.Control[iCurrent+16]=this.cbx_conslote
this.Control[iCurrent+17]=this.rb_detabultos
this.Control[iCurrent+18]=this.rb_detakilos
this.Control[iCurrent+19]=this.rb_proespvar
this.Control[iCurrent+20]=this.rb_espprovar
this.Control[iCurrent+21]=this.rb_espvarpro
this.Control[iCurrent+22]=this.gb_detalle
this.Control[iCurrent+23]=this.gb_ordena
this.Control[iCurrent+24]=this.st_10
this.Control[iCurrent+25]=this.st_12
this.Control[iCurrent+26]=this.st_14
this.Control[iCurrent+27]=this.rb_retiro
this.Control[iCurrent+28]=this.gb_3
this.Control[iCurrent+29]=this.st_13
this.Control[iCurrent+30]=this.rb_venta
this.Control[iCurrent+31]=this.st_16
this.Control[iCurrent+32]=this.uo_selespe
this.Control[iCurrent+33]=this.st_11
this.Control[iCurrent+34]=this.uo_selexpo
this.Control[iCurrent+35]=this.st_15
end on

on w_info_existencia_productor_calibre.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selprod)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.uo_selvari)
destroy(this.uo_selplta)
destroy(this.uo_selcama)
destroy(this.uo_selfrio)
destroy(this.uo_selcate)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.cbx_conslote)
destroy(this.rb_detabultos)
destroy(this.rb_detakilos)
destroy(this.rb_proespvar)
destroy(this.rb_espprovar)
destroy(this.rb_espvarpro)
destroy(this.gb_detalle)
destroy(this.gb_ordena)
destroy(this.st_10)
destroy(this.st_12)
destroy(this.st_14)
destroy(this.rb_retiro)
destroy(this.gb_3)
destroy(this.st_13)
destroy(this.rb_venta)
destroy(this.st_16)
destroy(this.uo_selespe)
destroy(this.st_11)
destroy(this.uo_selexpo)
destroy(this.st_15)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelExpo.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelPlta.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelCama.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelProd.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelEspe.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelVari.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelFrio.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelCate.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelExpo.Seleccion(True, False)
	
	uo_SelProd.Todos(True)

	uo_SelVari.Todos(True)
	
	uo_SelVari.cbx_Todos.Enabled	=	False

	uo_SelCama.Todos(True)
	
	uo_SelCama.cbx_Todos.Enabled	=	False
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_existencia_productor_calibre
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_productor_calibre
integer x = 1010
integer width = 2565
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_productor_calibre
integer x = 1010
integer width = 2565
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_productor_calibre
integer x = 1010
integer width = 2565
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_productor_calibre
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_productor_calibre
integer width = 2930
fontcharset fontcharset = ansi!
string text = "Informe Existencias por Productor y Calibre"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_productor_calibre
integer x = 3250
integer y = 712
integer taborder = 120
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila
Integer	li_Ordenamiento, li_ConsolidaEspecie, li_ConsolidaVariedad, &
			li_ConsolidaLote, li_Detalle, li_tipool

istr_info.titulo	= "EXISTENCIA DE FRUTA COMERCIAL"
istr_info.copias	= 1
	
OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_existencia_merc_interno_calibre"

vinf.dw_1.SetTransObject(sqlca)

IF rb_DetaBultos.Checked THEN li_Detalle = 1

IF rb_ProEspVar.Checked THEN
	li_Ordenamiento = 1
	
	vinf.dw_1.SetSort('expo_codigo A, plde_codigo A, cama_codigo A, ' + &
							'prod_codigo A, lofc_espcod A, vari_codigo A, ' + &
							'frio_tipofr A, cate_codigo A, lofc_lotefc A')
ELSEIF rb_EspProVar.Checked THEN
	li_Ordenamiento = 2
	
	vinf.dw_1.SetSort('expo_codigo A, plde_codigo A, cama_codigo A, ' + &
							'lofc_espcod A, prod_codigo A, vari_codigo A, ' + &
							'frio_tipofr A, cate_codigo A, lofc_lotefc A')
ELSEIF rb_EspVarPro.Checked THEN
	li_Ordenamiento = 3
	
	vinf.dw_1.SetSort('expo_codigo A, plde_codigo A, cama_codigo A, ' + &
							'lofc_espcod A, vari_codigo A, prod_codigo A, ' + &
							'frio_tipofr A, cate_codigo A, lofc_lotefc A')
END IF

IF uo_SelEspe.Codigo = -9 THEN
	uo_SelEspe.Codigo 	= -1
	li_ConsolidaEspecie	=	1
ELSE
	li_ConsolidaEspecie	=	0
END IF

IF uo_SelVari.Codigo = -9 THEN
	uo_SelVari.Codigo 	= -1
	li_ConsolidaVariedad	=	1
ELSE
	li_ConsolidaVariedad	=	0
END IF

IF cbx_ConsLote.Checked THEN li_ConsolidaLote = 1

IF rb_Venta.Checked THEN
	li_tipool = 1
ELSE
	li_tipool = 2
END IF	

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelExpo.Codigo, uo_SelPlta.Codigo, &
										uo_SelCama.Codigo, uo_SelProd.Codigo, &
										uo_SelEspe.Codigo, uo_SelVari.Codigo, &
										uo_SelFrio.Codigo, uo_SelCate.Codigo, &
										li_ConsolidaEspecie, li_ConsolidaVariedad, &
										li_ConsolidaLote,li_Detalle,li_Ordenamiento, li_tipool)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF
	
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_productor_calibre
integer x = 3250
integer y = 1076
integer taborder = 140
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type st_1 from statictext within w_info_existencia_productor_calibre
integer x = 251
integer y = 424
integer width = 1568
integer height = 432
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selprod from uo_seleccion_productor within w_info_existencia_productor_calibre
integer x = 677
integer y = 632
integer taborder = 10
boolean bringtotop = true
end type

on uo_selprod.destroy
call uo_seleccion_productor::destroy
end on

type st_2 from statictext within w_info_existencia_productor_calibre
integer x = 315
integer y = 652
integer width = 338
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_existencia_productor_calibre
integer x = 315
integer y = 456
integer width = 338
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Exportador"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_existencia_productor_calibre
integer x = 1879
integer y = 472
integer width = 320
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_existencia_productor_calibre
integer x = 1879
integer y = 652
integer width = 279
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type uo_selvari from uo_seleccion_variedad within w_info_existencia_productor_calibre
integer x = 2245
integer y = 652
integer taborder = 50
boolean bringtotop = true
end type

on uo_selvari.destroy
call uo_seleccion_variedad::destroy
end on

event ue_cambio();call super::ue_cambio;ConsolidaLote()
end event

type uo_selplta from uo_seleccion_plantas within w_info_existencia_productor_calibre
integer x = 677
integer y = 884
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio();call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_SelCama.Todos(True)
		
		uo_SelCama.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_SelCama.Filtra(This.Codigo)
		
		uo_SelCama.cbx_Todos.Enabled	=	True
		
END CHOOSE

ConsolidaLote()
end event

type uo_selcama from uo_seleccion_camara within w_info_existencia_productor_calibre
integer x = 677
integer y = 1080
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcama.destroy
call uo_seleccion_camara::destroy
end on

type uo_selfrio from uo_seleccion_tipofrio within w_info_existencia_productor_calibre
integer x = 2245
integer y = 884
integer taborder = 70
boolean bringtotop = true
end type

on uo_selfrio.destroy
call uo_seleccion_tipofrio::destroy
end on

type uo_selcate from uo_seleccion_categoria within w_info_existencia_productor_calibre
integer x = 2245
integer y = 1080
integer taborder = 80
boolean bringtotop = true
end type

on uo_selcate.destroy
call uo_seleccion_categoria::destroy
end on

type st_6 from statictext within w_info_existencia_productor_calibre
integer x = 315
integer y = 884
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_existencia_productor_calibre
integer x = 315
integer y = 1080
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cámara"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_existencia_productor_calibre
integer x = 1879
integer y = 884
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Frio"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_existencia_productor_calibre
integer x = 1879
integer y = 1080
integer width = 302
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Categoria"
boolean focusrectangle = false
end type

type cbx_conslote from checkbox within w_info_existencia_productor_calibre
integer x = 357
integer y = 1360
integer width = 489
integer height = 72
integer taborder = 90
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolida Lote"
boolean lefttext = true
end type

type rb_detabultos from radiobutton within w_info_existencia_productor_calibre
integer x = 677
integer y = 1540
integer width = 402
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Bultos"
boolean checked = true
end type

type rb_detakilos from radiobutton within w_info_existencia_productor_calibre
integer x = 1266
integer y = 1536
integer width = 402
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Kilos"
end type

type rb_proespvar from radiobutton within w_info_existencia_productor_calibre
integer x = 2048
integer y = 1360
integer width = 928
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor  /  Especie  /  Variedad"
boolean checked = true
end type

type rb_espprovar from radiobutton within w_info_existencia_productor_calibre
integer x = 2048
integer y = 1440
integer width = 928
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie  /  Productor  /  Variedad"
end type

type rb_espvarpro from radiobutton within w_info_existencia_productor_calibre
integer x = 2048
integer y = 1520
integer width = 928
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie  /  Variedad  /  Productor"
end type

type gb_detalle from groupbox within w_info_existencia_productor_calibre
integer x = 315
integer y = 1484
integer width = 1445
integer height = 140
integer taborder = 100
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
string text = "Tipo de Detalle"
end type

type gb_ordena from groupbox within w_info_existencia_productor_calibre
integer x = 1879
integer y = 1308
integer width = 1243
integer height = 308
integer taborder = 110
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo de Ordenamiento"
end type

type st_10 from statictext within w_info_existencia_productor_calibre
integer x = 251
integer y = 856
integer width = 1568
integer height = 432
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_existencia_productor_calibre
integer x = 1819
integer y = 856
integer width = 1358
integer height = 432
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_existencia_productor_calibre
integer x = 251
integer y = 1472
integer width = 1568
integer height = 192
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_retiro from radiobutton within w_info_existencia_productor_calibre
integer x = 1472
integer y = 1360
integer width = 256
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Retiro"
end type

type gb_3 from groupbox within w_info_existencia_productor_calibre
integer x = 315
integer y = 1296
integer width = 1440
integer height = 152
integer taborder = 130
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Lote"
end type

type st_13 from statictext within w_info_existencia_productor_calibre
integer x = 251
integer y = 1288
integer width = 1568
integer height = 184
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_venta from radiobutton within w_info_existencia_productor_calibre
integer x = 1198
integer y = 1360
integer width = 261
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Venta"
boolean checked = true
end type

type st_16 from statictext within w_info_existencia_productor_calibre
integer x = 914
integer y = 1364
integer width = 233
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tip. Pool"
boolean focusrectangle = false
end type

type uo_selespe from uo_seleccion_especie within w_info_existencia_productor_calibre
integer x = 2245
integer y = 472
integer taborder = 40
end type

on uo_selespe.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio();IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_SelVari.Todos(True)
		
		uo_SelVari.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_SelVari.Filtra(This.Codigo)
		
		uo_SelVari.cbx_Todos.Enabled	=	True
		
END CHOOSE

ConsolidaLote()


end event

type st_11 from statictext within w_info_existencia_productor_calibre
integer x = 1819
integer y = 424
integer width = 1358
integer height = 432
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selexpo from uo_seleccion_clientesprod within w_info_existencia_productor_calibre
integer x = 677
integer y = 436
integer height = 188
integer taborder = 60
boolean bringtotop = true
end type

on uo_selexpo.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_15 from statictext within w_info_existencia_productor_calibre
integer x = 1819
integer y = 1288
integer width = 1358
integer height = 376
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

