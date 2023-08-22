$PBExportHeader$w_info_acumulado_fruta_comercial.srw
$PBExportComments$Informe de Existencia de Fruta Comercial en Camara por Productor y Calibre.
forward
global type w_info_acumulado_fruta_comercial from w_para_informes
end type
type st_1 from statictext within w_info_acumulado_fruta_comercial
end type
type uo_selprod from uo_seleccion_productor within w_info_acumulado_fruta_comercial
end type
type st_2 from statictext within w_info_acumulado_fruta_comercial
end type
type st_4 from statictext within w_info_acumulado_fruta_comercial
end type
type st_5 from statictext within w_info_acumulado_fruta_comercial
end type
type uo_selvari from uo_seleccion_variedad within w_info_acumulado_fruta_comercial
end type
type uo_selplta from uo_seleccion_plantas within w_info_acumulado_fruta_comercial
end type
type uo_selcate from uo_seleccion_categoria within w_info_acumulado_fruta_comercial
end type
type st_6 from statictext within w_info_acumulado_fruta_comercial
end type
type st_9 from statictext within w_info_acumulado_fruta_comercial
end type
type rb_proespvar from radiobutton within w_info_acumulado_fruta_comercial
end type
type rb_espprovar from radiobutton within w_info_acumulado_fruta_comercial
end type
type rb_espvarpro from radiobutton within w_info_acumulado_fruta_comercial
end type
type gb_ordena from groupbox within w_info_acumulado_fruta_comercial
end type
type st_10 from statictext within w_info_acumulado_fruta_comercial
end type
type st_12 from statictext within w_info_acumulado_fruta_comercial
end type
type uo_selespe from uo_seleccion_especie within w_info_acumulado_fruta_comercial
end type
type st_11 from statictext within w_info_acumulado_fruta_comercial
end type
type st_13 from statictext within w_info_acumulado_fruta_comercial
end type
type em_1 from editmask within w_info_acumulado_fruta_comercial
end type
type em_2 from editmask within w_info_acumulado_fruta_comercial
end type
type st_14 from statictext within w_info_acumulado_fruta_comercial
end type
end forward

global type w_info_acumulado_fruta_comercial from w_para_informes
integer x = 0
integer y = 0
integer width = 3488
integer height = 1256
st_1 st_1
uo_selprod uo_selprod
st_2 st_2
st_4 st_4
st_5 st_5
uo_selvari uo_selvari
uo_selplta uo_selplta
uo_selcate uo_selcate
st_6 st_6
st_9 st_9
rb_proespvar rb_proespvar
rb_espprovar rb_espprovar
rb_espvarpro rb_espvarpro
gb_ordena gb_ordena
st_10 st_10
st_12 st_12
uo_selespe uo_selespe
st_11 st_11
st_13 st_13
em_1 em_1
em_2 em_2
st_14 st_14
end type
global w_info_acumulado_fruta_comercial w_info_acumulado_fruta_comercial

forward prototypes
public subroutine consolidalote ()
end prototypes

public subroutine consolidalote ();//IF uo_SelProd.Codigo = -9 OR &
//	uo_SelEspe.Codigo = -9 OR &
//	uo_SelVari.Codigo = -9 THEN
//	cbx_ConsLote.Checked	=	False
//	cbx_ConsLote.Enabled	=	False
//ELSE
//	cbx_ConsLote.Checked	=	True
//	cbx_ConsLote.Enabled	=	True
//END IF
//
//RETURN
end subroutine

on w_info_acumulado_fruta_comercial.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selprod=create uo_selprod
this.st_2=create st_2
this.st_4=create st_4
this.st_5=create st_5
this.uo_selvari=create uo_selvari
this.uo_selplta=create uo_selplta
this.uo_selcate=create uo_selcate
this.st_6=create st_6
this.st_9=create st_9
this.rb_proespvar=create rb_proespvar
this.rb_espprovar=create rb_espprovar
this.rb_espvarpro=create rb_espvarpro
this.gb_ordena=create gb_ordena
this.st_10=create st_10
this.st_12=create st_12
this.uo_selespe=create uo_selespe
this.st_11=create st_11
this.st_13=create st_13
this.em_1=create em_1
this.em_2=create em_2
this.st_14=create st_14
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selprod
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_4
this.Control[iCurrent+5]=this.st_5
this.Control[iCurrent+6]=this.uo_selvari
this.Control[iCurrent+7]=this.uo_selplta
this.Control[iCurrent+8]=this.uo_selcate
this.Control[iCurrent+9]=this.st_6
this.Control[iCurrent+10]=this.st_9
this.Control[iCurrent+11]=this.rb_proespvar
this.Control[iCurrent+12]=this.rb_espprovar
this.Control[iCurrent+13]=this.rb_espvarpro
this.Control[iCurrent+14]=this.gb_ordena
this.Control[iCurrent+15]=this.st_10
this.Control[iCurrent+16]=this.st_12
this.Control[iCurrent+17]=this.uo_selespe
this.Control[iCurrent+18]=this.st_11
this.Control[iCurrent+19]=this.st_13
this.Control[iCurrent+20]=this.em_1
this.Control[iCurrent+21]=this.em_2
this.Control[iCurrent+22]=this.st_14
end on

on w_info_acumulado_fruta_comercial.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selprod)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.uo_selvari)
destroy(this.uo_selplta)
destroy(this.uo_selcate)
destroy(this.st_6)
destroy(this.st_9)
destroy(this.rb_proespvar)
destroy(this.rb_espprovar)
destroy(this.rb_espvarpro)
destroy(this.gb_ordena)
destroy(this.st_10)
destroy(this.st_12)
destroy(this.uo_selespe)
destroy(this.st_11)
destroy(this.st_13)
destroy(this.em_1)
destroy(this.em_2)
destroy(this.st_14)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelPlta.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelProd.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelEspe.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelVari.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelCate.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelProd.Todos(True)

	uo_SelVari.Todos(True)
	
	uo_SelVari.cbx_Todos.Enabled	=	False
END IF

em_1.text = '01/01/' + String(Year(Today()))
em_2.text = String(Today(),'dd/mm/yyyy')
end event

type st_titulo from w_para_informes`st_titulo within w_info_acumulado_fruta_comercial
integer x = 78
integer y = 68
integer width = 2930
fontcharset fontcharset = ansi!
string text = "Informe Acumulado ded Fruta Comercial"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_acumulado_fruta_comercial
integer x = 3136
integer y = 384
integer taborder = 120
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila
Integer	li_Ordenamiento, li_ConsolidaEspecie, li_ConsolidaVariedad
Date		ld_fecini, ld_fecter
			
istr_info.titulo	= "ACUMULADO DE FRUTA COMERCIAL"
istr_info.copias	= 1

ld_fecini = Date(em_1.text)
ld_fecter = Date(em_2.text)

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_acumulado_fruta_comercial"

vinf.dw_1.SetTransObject(sqlca)

IF rb_ProEspVar.Checked THEN
	li_Ordenamiento = 1
		
	vinf.dw_1.SetSort('mfco_fecmov A, lofc_pltcod A, prod_codigo A, lofc_espcod A, ' + &
							'vari_codigo A, cate_codigo A')
ELSEIF rb_EspProVar.Checked THEN
	li_Ordenamiento = 2
	
	vinf.dw_1.SetSort('mfco_fecmov A, lofc_pltcod A, lofc_espcod A, prod_codigo A,' + &
							'vari_codigo A, cate_codigo A')
ELSEIF rb_EspVarPro.Checked THEN
	li_Ordenamiento = 3
	
	vinf.dw_1.SetSort('mfco_fecmov A, lofc_pltcod A, lofc_espcod A, vari_codigo A, ' + &
							'prod_codigo A, cate_codigo A')
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

IF uo_SelPlta.Codigo <> -1 OR uo_SelPlta.Codigo <> -9 THEN
	vinf.dw_1.Object.planta.text = uo_SelPlta.nombre
ELSE
	IF uo_SelPlta.Codigo = -1 THEN
		vinf.dw_1.Object.planta.text = 'Todas'
	ELSE
		vinf.dw_1.Object.planta.text = 'Consolidada'
	END IF
END IF

IF uo_SelProd.Codigo <> -1 OR uo_SelProd.Codigo <> -9 THEN
	vinf.dw_1.Object.productor.text = uo_SelProd.nombre
ELSE
	IF uo_SelProd.Codigo = -1 THEN
		vinf.dw_1.Object.productor.text = 'Todos'
	ELSE
		vinf.dw_1.Object.productor.text = 'Consolidada'
	END IF
END IF

IF uo_SelEspe.Codigo <> -1 OR uo_SelEspe.Codigo <> -9 THEN
	vinf.dw_1.Object.especie.text = uo_SelEspe.nombre
ELSE
	IF uo_SelEspe.Codigo = -1 THEN
		vinf.dw_1.Object.especie.text = 'Todas'
	ELSE
		vinf.dw_1.Object.especie.text = 'Consolidada'
	END IF
END IF

IF uo_SelVari.Codigo <> -1 OR uo_SelVari.Codigo <> -9 THEN
	vinf.dw_1.Object.variedad.text = uo_SelVari.nombre
ELSE
	IF uo_SelVari.Codigo = -1 THEN
		vinf.dw_1.Object.variedad.text = 'Todas'
	ELSE
		vinf.dw_1.Object.variedad.text = 'Consolidada'
	END IF
END IF

IF uo_SelCate.Codigo <> -1 OR uo_SelCate.Codigo <> -9 THEN
	vinf.dw_1.Object.categoria.text = uo_SelCate.nombre
ELSE
	IF uo_SelCate.Codigo = -1 THEN
		vinf.dw_1.Object.categoria.text = 'Todas'
	ELSE
		vinf.dw_1.Object.categoria.text = 'Consolidada'
	END IF
END IF

vinf.dw_1.Object.fechas.text = ' desde ' + String(ld_fecini,'dd/mm/yyyy') + ' a '+ string(ld_fecter,'dd/mm/yyyy')

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelPlta.Codigo, uo_SelProd.Codigo,	uo_SelEspe.Codigo, &
										uo_SelVari.Codigo, uo_SelCate.Codigo, ld_fecini, &
										ld_fecter, li_ConsolidaEspecie, li_ConsolidaVariedad)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	vinf.dw_1.Sort()
	vinf.Visible	= True
	vinf.Enabled	= True
	
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_acumulado_fruta_comercial
integer x = 3136
integer y = 748
integer taborder = 130
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type st_1 from statictext within w_info_acumulado_fruta_comercial
integer x = 78
integer y = 216
integer width = 1568
integer height = 432
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selprod from uo_seleccion_productor within w_info_acumulado_fruta_comercial
integer x = 503
integer y = 444
integer taborder = 20
boolean bringtotop = true
end type

on uo_selprod.destroy
call uo_seleccion_productor::destroy
end on

type st_2 from statictext within w_info_acumulado_fruta_comercial
integer x = 142
integer y = 444
integer width = 338
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Productor"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_acumulado_fruta_comercial
integer x = 1705
integer y = 264
integer width = 320
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Especie"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_acumulado_fruta_comercial
integer x = 1705
integer y = 444
integer width = 279
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type uo_selvari from uo_seleccion_variedad within w_info_acumulado_fruta_comercial
integer x = 2071
integer y = 444
integer taborder = 60
boolean bringtotop = true
end type

on uo_selvari.destroy
call uo_seleccion_variedad::destroy
end on

type uo_selplta from uo_seleccion_plantas within w_info_acumulado_fruta_comercial
integer x = 503
integer y = 256
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcate from uo_seleccion_categoria within w_info_acumulado_fruta_comercial
integer x = 517
integer y = 708
integer taborder = 80
boolean bringtotop = true
end type

on uo_selcate.destroy
call uo_seleccion_categoria::destroy
end on

type st_6 from statictext within w_info_acumulado_fruta_comercial
integer x = 142
integer y = 256
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_acumulado_fruta_comercial
integer x = 151
integer y = 708
integer width = 302
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Categoria"
boolean focusrectangle = false
end type

type rb_proespvar from radiobutton within w_info_acumulado_fruta_comercial
integer x = 1769
integer y = 740
integer width = 1129
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha  /  Productor  /  Especie  /  Variedad"
boolean checked = true
borderstyle borderstyle = stylelowered!
end type

type rb_espprovar from radiobutton within w_info_acumulado_fruta_comercial
integer x = 1769
integer y = 820
integer width = 1129
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha  /  Especie  /  Productor  /  Variedad"
borderstyle borderstyle = stylelowered!
end type

type rb_espvarpro from radiobutton within w_info_acumulado_fruta_comercial
integer x = 1769
integer y = 900
integer width = 1129
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha  /  Especie  /  Variedad  /  Productor"
borderstyle borderstyle = stylelowered!
end type

type gb_ordena from groupbox within w_info_acumulado_fruta_comercial
integer x = 1696
integer y = 688
integer width = 1243
integer height = 308
integer taborder = 110
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Tipo de Ordenamiento"
borderstyle borderstyle = stylelowered!
end type

type st_10 from statictext within w_info_acumulado_fruta_comercial
integer x = 78
integer y = 648
integer width = 1568
integer height = 432
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_acumulado_fruta_comercial
integer x = 1646
integer y = 648
integer width = 1358
integer height = 432
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespe from uo_seleccion_especie within w_info_acumulado_fruta_comercial
integer x = 2071
integer y = 264
integer taborder = 50
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
end event

type st_11 from statictext within w_info_acumulado_fruta_comercial
integer x = 1646
integer y = 216
integer width = 1358
integer height = 432
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_acumulado_fruta_comercial
integer x = 151
integer y = 928
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha Desde"
boolean focusrectangle = false
end type

type em_1 from editmask within w_info_acumulado_fruta_comercial
integer x = 517
integer y = 916
integer width = 288
integer height = 88
integer taborder = 140
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_2 from editmask within w_info_acumulado_fruta_comercial
integer x = 1166
integer y = 916
integer width = 288
integer height = 88
integer taborder = 150
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_14 from statictext within w_info_acumulado_fruta_comercial
integer x = 837
integer y = 928
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha Hasta"
boolean focusrectangle = false
end type

