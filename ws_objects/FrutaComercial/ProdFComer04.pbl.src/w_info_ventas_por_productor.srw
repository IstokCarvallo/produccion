$PBExportHeader$w_info_ventas_por_productor.srw
$PBExportComments$Informe de Existencia de Fruta Comercial en Camara por Productor y Calibre.
forward
global type w_info_ventas_por_productor from w_para_informes
end type
type gb_detalle from groupbox within w_info_ventas_por_productor
end type
type st_1 from statictext within w_info_ventas_por_productor
end type
type st_2 from statictext within w_info_ventas_por_productor
end type
type st_4 from statictext within w_info_ventas_por_productor
end type
type st_5 from statictext within w_info_ventas_por_productor
end type
type uo_selvari from uo_seleccion_variedad within w_info_ventas_por_productor
end type
type st_6 from statictext within w_info_ventas_por_productor
end type
type rb_cliente from radiobutton within w_info_ventas_por_productor
end type
type rb_guia from radiobutton within w_info_ventas_por_productor
end type
type gb_ordena from groupbox within w_info_ventas_por_productor
end type
type uo_selespe from uo_seleccion_especie within w_info_ventas_por_productor
end type
type st_11 from statictext within w_info_ventas_por_productor
end type
type st_13 from statictext within w_info_ventas_por_productor
end type
type em_1 from editmask within w_info_ventas_por_productor
end type
type em_2 from editmask within w_info_ventas_por_productor
end type
type st_14 from statictext within w_info_ventas_por_productor
end type
type em_guia from editmask within w_info_ventas_por_productor
end type
type cbx_fectodos from checkbox within w_info_ventas_por_productor
end type
type cbx_feccons from checkbox within w_info_ventas_por_productor
end type
type cbx_todosguia from checkbox within w_info_ventas_por_productor
end type
type cbx_consguia from checkbox within w_info_ventas_por_productor
end type
type gb_3 from groupbox within w_info_ventas_por_productor
end type
type st_10 from statictext within w_info_ventas_por_productor
end type
type rb_infprod from radiobutton within w_info_ventas_por_productor
end type
type rb_infcli from radiobutton within w_info_ventas_por_productor
end type
type gb_informe from groupbox within w_info_ventas_por_productor
end type
type st_12 from statictext within w_info_ventas_por_productor
end type
type rb_grupo from radiobutton within w_info_ventas_por_productor
end type
type rb_detalle from radiobutton within w_info_ventas_por_productor
end type
type uo_selecli from uo_seleccion_clientesprod within w_info_ventas_por_productor
end type
type st_3 from statictext within w_info_ventas_por_productor
end type
type uo_seleent from uo_seleccion_productor within w_info_ventas_por_productor
end type
type ddlb_tipodocto from dropdownlistbox within w_info_ventas_por_productor
end type
type st_7 from statictext within w_info_ventas_por_productor
end type
end forward

global type w_info_ventas_por_productor from w_para_informes
integer x = 0
integer y = 0
integer width = 3685
integer height = 1620
string title = "Informe de Ventas por Cliente"
boolean maxbox = false
gb_detalle gb_detalle
st_1 st_1
st_2 st_2
st_4 st_4
st_5 st_5
uo_selvari uo_selvari
st_6 st_6
rb_cliente rb_cliente
rb_guia rb_guia
gb_ordena gb_ordena
uo_selespe uo_selespe
st_11 st_11
st_13 st_13
em_1 em_1
em_2 em_2
st_14 st_14
em_guia em_guia
cbx_fectodos cbx_fectodos
cbx_feccons cbx_feccons
cbx_todosguia cbx_todosguia
cbx_consguia cbx_consguia
gb_3 gb_3
st_10 st_10
rb_infprod rb_infprod
rb_infcli rb_infcli
gb_informe gb_informe
st_12 st_12
rb_grupo rb_grupo
rb_detalle rb_detalle
uo_selecli uo_selecli
st_3 st_3
uo_seleent uo_seleent
ddlb_tipodocto ddlb_tipodocto
st_7 st_7
end type
global w_info_ventas_por_productor w_info_ventas_por_productor

type variables
str_mant    			istr_mant
DataWindowChild	idwc_cliente
INteger				ii_Tipo
end variables

forward prototypes
public function boolean noexistecliente (integer cliente)
public function boolean existe_guia (integer ai_codigo, long al_guia)
end prototypes

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 		clie_nombre
INTO			:ls_nombre
FROM 		dba.clientesprod
WHERE		clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public function boolean existe_guia (integer ai_codigo, long al_guia);Long ll_guia

SELECT Distinct mfco_guisii
INTO   :ll_guia
FROM   dba.spro_movtofrutacomenca 
WHERE  :ai_codigo in (-1,-9,clie_codigo)
AND    :al_guia in (-1,-9,mfco_guisii);

IF IsNull(ll_guia) OR ll_guia = 0 THEN
	MessageBox("Atención","Guía No Existe, Ingrese Otra")
	RETURN FALSE
ELSEIF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Spro_movtofrutacomenca")
	RETURN FALSE
END IF
RETURN TRUE
end function

on w_info_ventas_por_productor.create
int iCurrent
call super::create
this.gb_detalle=create gb_detalle
this.st_1=create st_1
this.st_2=create st_2
this.st_4=create st_4
this.st_5=create st_5
this.uo_selvari=create uo_selvari
this.st_6=create st_6
this.rb_cliente=create rb_cliente
this.rb_guia=create rb_guia
this.gb_ordena=create gb_ordena
this.uo_selespe=create uo_selespe
this.st_11=create st_11
this.st_13=create st_13
this.em_1=create em_1
this.em_2=create em_2
this.st_14=create st_14
this.em_guia=create em_guia
this.cbx_fectodos=create cbx_fectodos
this.cbx_feccons=create cbx_feccons
this.cbx_todosguia=create cbx_todosguia
this.cbx_consguia=create cbx_consguia
this.gb_3=create gb_3
this.st_10=create st_10
this.rb_infprod=create rb_infprod
this.rb_infcli=create rb_infcli
this.gb_informe=create gb_informe
this.st_12=create st_12
this.rb_grupo=create rb_grupo
this.rb_detalle=create rb_detalle
this.uo_selecli=create uo_selecli
this.st_3=create st_3
this.uo_seleent=create uo_seleent
this.ddlb_tipodocto=create ddlb_tipodocto
this.st_7=create st_7
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_detalle
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_4
this.Control[iCurrent+5]=this.st_5
this.Control[iCurrent+6]=this.uo_selvari
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.rb_cliente
this.Control[iCurrent+9]=this.rb_guia
this.Control[iCurrent+10]=this.gb_ordena
this.Control[iCurrent+11]=this.uo_selespe
this.Control[iCurrent+12]=this.st_11
this.Control[iCurrent+13]=this.st_13
this.Control[iCurrent+14]=this.em_1
this.Control[iCurrent+15]=this.em_2
this.Control[iCurrent+16]=this.st_14
this.Control[iCurrent+17]=this.em_guia
this.Control[iCurrent+18]=this.cbx_fectodos
this.Control[iCurrent+19]=this.cbx_feccons
this.Control[iCurrent+20]=this.cbx_todosguia
this.Control[iCurrent+21]=this.cbx_consguia
this.Control[iCurrent+22]=this.gb_3
this.Control[iCurrent+23]=this.st_10
this.Control[iCurrent+24]=this.rb_infprod
this.Control[iCurrent+25]=this.rb_infcli
this.Control[iCurrent+26]=this.gb_informe
this.Control[iCurrent+27]=this.st_12
this.Control[iCurrent+28]=this.rb_grupo
this.Control[iCurrent+29]=this.rb_detalle
this.Control[iCurrent+30]=this.uo_selecli
this.Control[iCurrent+31]=this.st_3
this.Control[iCurrent+32]=this.uo_seleent
this.Control[iCurrent+33]=this.ddlb_tipodocto
this.Control[iCurrent+34]=this.st_7
end on

on w_info_ventas_por_productor.destroy
call super::destroy
destroy(this.gb_detalle)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.uo_selvari)
destroy(this.st_6)
destroy(this.rb_cliente)
destroy(this.rb_guia)
destroy(this.gb_ordena)
destroy(this.uo_selespe)
destroy(this.st_11)
destroy(this.st_13)
destroy(this.em_1)
destroy(this.em_2)
destroy(this.st_14)
destroy(this.em_guia)
destroy(this.cbx_fectodos)
destroy(this.cbx_feccons)
destroy(this.cbx_todosguia)
destroy(this.cbx_consguia)
destroy(this.gb_3)
destroy(this.st_10)
destroy(this.rb_infprod)
destroy(this.rb_infcli)
destroy(this.gb_informe)
destroy(this.st_12)
destroy(this.rb_grupo)
destroy(this.rb_detalle)
destroy(this.uo_selecli)
destroy(this.st_3)
destroy(this.uo_seleent)
destroy(this.ddlb_tipodocto)
destroy(this.st_7)
end on

event open;call super::open;Boolean	lb_Cerrar


IF IsNull(uo_SeleCli.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SeleEnt.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspe.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelVari.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelVari.Todos(True)
	uo_SelVari.cbx_Todos.Enabled	=	False
	em_1.text = '01/01/' + String(Year(Today()))
	em_2.text = String(Today(),'dd/mm/yyyy')
	istr_mant.argumento[1] = ""	
	ii_Tipo = -1
	ddlb_tipodocto.SelectItem(3)
END IF



end event

type pb_excel from w_para_informes`pb_excel within w_info_ventas_por_productor
integer x = 3232
integer y = 332
end type

type st_computador from w_para_informes`st_computador within w_info_ventas_por_productor
integer x = 1015
integer width = 2560
end type

type st_usuario from w_para_informes`st_usuario within w_info_ventas_por_productor
integer x = 1015
integer width = 2560
end type

type st_temporada from w_para_informes`st_temporada within w_info_ventas_por_productor
integer x = 1015
integer width = 2560
end type

type p_logo from w_para_informes`p_logo within w_info_ventas_por_productor
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_ventas_por_productor
integer width = 2930
string text = "Ventas por Productor"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ventas_por_productor
integer x = 3232
integer y = 668
integer taborder = 150
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila, ll_guia
Integer	li_agrupa
Date		ld_fecini, ld_fecter

istr_info.titulo	= "VENTAS POR CLIENTES"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_ventas_solo_por_productor"
vinf.dw_1.SetTransObject(sqlca)

IF rb_cliente.Checked AND rb_infcli.checked THEN
	vinf.dw_1.SetSort('clpr_rut A, mfco_guisii A, lofc_espcod A, lofc_lotefc A, fgmb_nrotar A')
	vinf.dw_1.Sort()
ELSEIF rb_guia.Checked AND rb_infcli.checked THEN
	vinf.dw_1.SetSort('mfco_guisii A, clpr_rut A, lofc_espcod A, lofc_lotefc A, fgmb_nrotar A')
	vinf.dw_1.Sort()
END IF

//Guía Despacho
IF istr_mant.argumento[1] = "" THEN
	MessageBox("Atención","Debe ingresar una Guía de despacho")
	em_guia.SetFocus()
	RETURN
ELSE
	ll_guia   = Long(istr_mant.argumento[1])
END IF

//Fecha Movimiento
IF em_1.text = '00/00/0000' OR em_2.text = '00/00/0000' THEN
	MessageBox("Atención","Debe ingresar fechas de movimiento")
	RETURN
ELSEIF Date(em_2.text) <= Date(em_1.text) THEN
	MessageBox("Error de consistencia","Fecha termino no puede ser menor a fecha inicio",StopSign!)
	em_2.text = ""
	em_2.SetFocus()
	RETURN
ELSE	
	ld_fecini = Date(em_1.text)
	ld_fecter = Date(em_2.text)
END IF

IF rb_grupo.Checked THEN
	li_agrupa = 1
ELSEIF rb_detalle.Checked THEN
	li_agrupa = 2
END IF

ll_Fila	=	vinf.dw_1.Retrieve(uo_SeleCli.Codigo,uo_SelEspe.Codigo,uo_SelVari.Codigo,ll_guia,&
                               ld_fecini,ld_fecter,li_agrupa, uo_SeleEnt.Codigo, ii_Tipo)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("fechadesde.text = '" + String(ld_fecini,'dd/mm/yyyy') + "'")
	vinf.dw_1.Modify("fechahasta.text = '" + String(ld_fecter,'dd/mm/yyyy') + "'")	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_ventas_por_productor
integer x = 3232
integer y = 1032
integer taborder = 160
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type gb_detalle from groupbox within w_info_ventas_por_productor
integer x = 2770
integer y = 1072
integer width = 379
integer height = 280
integer taborder = 140
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Bins"
end type

type st_1 from statictext within w_info_ventas_por_productor
integer x = 256
integer y = 416
integer width = 1568
integer height = 636
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

type st_2 from statictext within w_info_ventas_por_productor
integer x = 329
integer y = 728
integer width = 421
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
string text = "Tipo Documento"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_ventas_por_productor
integer x = 1865
integer y = 736
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

type st_5 from statictext within w_info_ventas_por_productor
integer x = 1865
integer y = 956
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

type uo_selvari from uo_seleccion_variedad within w_info_ventas_por_productor
integer x = 2249
integer y = 864
integer height = 172
integer taborder = 70
boolean bringtotop = true
end type

on uo_selvari.destroy
call uo_seleccion_variedad::destroy
end on

type st_6 from statictext within w_info_ventas_por_productor
integer x = 329
integer y = 556
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
string text = "Cliente"
boolean focusrectangle = false
end type

type rb_cliente from radiobutton within w_info_ventas_por_productor
boolean visible = false
integer x = 2295
integer y = 1144
integer width = 439
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
string text = "Productor"
boolean checked = true
end type

type rb_guia from radiobutton within w_info_ventas_por_productor
boolean visible = false
integer x = 2286
integer y = 1236
integer width = 439
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
string text = "Guía Despacho"
end type

type gb_ordena from groupbox within w_info_ventas_por_productor
boolean visible = false
integer x = 2258
integer y = 1072
integer width = 503
integer height = 280
integer taborder = 130
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Ordenamiento"
end type

type uo_selespe from uo_seleccion_especie within w_info_ventas_por_productor
integer x = 2249
integer y = 640
integer taborder = 60
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

type st_11 from statictext within w_info_ventas_por_productor
integer x = 1824
integer y = 416
integer width = 1358
integer height = 636
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

type st_13 from statictext within w_info_ventas_por_productor
integer x = 507
integer y = 1232
integer width = 210
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
string text = "Desde"
boolean focusrectangle = false
end type

type em_1 from editmask within w_info_ventas_por_productor
integer x = 750
integer y = 1220
integer width = 288
integer height = 88
integer taborder = 100
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

type em_2 from editmask within w_info_ventas_por_productor
integer x = 1344
integer y = 1220
integer width = 288
integer height = 88
integer taborder = 110
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

type st_14 from statictext within w_info_ventas_por_productor
integer x = 1143
integer y = 1232
integer width = 206
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
string text = " Hasta"
boolean focusrectangle = false
end type

type em_guia from editmask within w_info_ventas_por_productor
integer x = 750
integer y = 948
integer width = 261
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "############"
end type

event modified;IF Not IsNull(em_guia.text) AND Not em_guia.text = "" THEN
	IF Not Existe_guia(uo_SeleCli.Codigo,Long(This.text)) THEN
		This.Text = ""
		istr_mant.argumento[1] = ""
		This.SetFocus()
		RETURN 1
	ELSE
		istr_mant.argumento[1] = This.text
	END IF
END IF

end event

type cbx_fectodos from checkbox within w_info_ventas_por_productor
integer x = 745
integer y = 1124
integer width = 343
integer height = 72
integer taborder = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	em_1.Enabled	=	FALSE
	em_2.Enabled	=	FALSE
   em_1.text   	=	'01/01/1900'
	em_2.text   	=	String(Today())
	cbx_feccons.enabled   =  TRUE
	cbx_feccons.checked   =  FALSE
ELSE
	em_1.Enabled	=	TRUE
	em_2.Enabled	=	TRUE
	em_1.SetFocus()
END IF
RETURN 0

end event

type cbx_feccons from checkbox within w_info_ventas_por_productor
integer x = 1253
integer y = 1124
integer width = 384
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
string text = "Consolidado"
end type

event clicked;IF This.Checked THEN
	em_1.Enabled	=	FALSE
	em_2.Enabled	=	FALSE
   em_1.text   	=	'01/01/1900'
	em_2.text   	=	String(Today())
	cbx_fectodos.Checked = False
ELSE
	em_1.Enabled	=	TRUE
	em_2.Enabled	=	TRUE
	em_1.SetFocus()
END IF
RETURN 0

end event

type cbx_todosguia from checkbox within w_info_ventas_por_productor
integer x = 745
integer y = 864
integer width = 343
integer height = 72
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[1] = '-1'
	cbx_consguia.Enabled = FALSE
	em_guia.Enabled = FALSE
ELSE
	cbx_consguia.Enabled = TRUE
	em_guia.Enabled = TRUE
	istr_mant.argumento[1] = ''
END IF
end event

type cbx_consguia from checkbox within w_info_ventas_por_productor
integer x = 1225
integer y = 868
integer width = 384
integer height = 72
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[1] = '-9'
	cbx_todosguia.Enabled = FALSE
	em_guia.Enabled = FALSE
ELSE
	cbx_todosguia.Enabled = TRUE
	em_guia.Enabled = TRUE
	istr_mant.argumento[1] = ''
END IF
end event

type gb_3 from groupbox within w_info_ventas_por_productor
integer x = 315
integer y = 1068
integer width = 1431
integer height = 284
integer taborder = 170
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Movimiento"
end type

type st_10 from statictext within w_info_ventas_por_productor
integer x = 256
integer y = 1056
integer width = 1568
integer height = 328
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

type rb_infprod from radiobutton within w_info_ventas_por_productor
boolean visible = false
integer x = 1902
integer y = 1240
integer width = 320
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
string text = "Productor"
boolean checked = true
end type

event clicked;rb_cliente.Checked 	= 	False
rb_guia.Checked 		= 	False
rb_cliente.Enabled 	= 	False
rb_guia.Enabled 		= 	False
gb_ordena.Enabled	=	False
end event

type rb_infcli from radiobutton within w_info_ventas_por_productor
boolean visible = false
integer x = 1902
integer y = 1148
integer width = 320
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
string text = "Cliente"
end type

event clicked;rb_cliente.Checked 	= 	True
rb_cliente.Enabled 	= 	True
rb_guia.Enabled 		= 	True
gb_ordena.Enabled	=	True
end event

type gb_informe from groupbox within w_info_ventas_por_productor
boolean visible = false
integer x = 1870
integer y = 1072
integer width = 379
integer height = 280
integer taborder = 120
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Informe"
end type

type st_12 from statictext within w_info_ventas_por_productor
integer x = 1824
integer y = 1056
integer width = 1358
integer height = 328
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

type rb_grupo from radiobutton within w_info_ventas_por_productor
integer x = 2816
integer y = 1148
integer width = 315
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Grupo"
boolean checked = true
end type

type rb_detalle from radiobutton within w_info_ventas_por_productor
integer x = 2816
integer y = 1240
integer width = 315
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Detalle"
end type

type uo_selecli from uo_seleccion_clientesprod within w_info_ventas_por_productor
event destroy ( )
integer x = 745
integer y = 456
integer height = 172
integer taborder = 10
boolean bringtotop = true
end type

on uo_selecli.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_3 from statictext within w_info_ventas_por_productor
integer x = 1865
integer y = 564
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
string text = "Productor"
boolean focusrectangle = false
end type

type uo_seleent from uo_seleccion_productor within w_info_ventas_por_productor
integer x = 2249
integer y = 452
integer taborder = 80
boolean bringtotop = true
end type

on uo_seleent.destroy
call uo_seleccion_productor::destroy
end on

type ddlb_tipodocto from dropdownlistbox within w_info_ventas_por_productor
integer x = 745
integer y = 720
integer width = 896
integer height = 352
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
boolean sorted = false
string item[] = {"Guia Despacho","Boletas","Ambos"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;If Index = 1 Then
	ii_Tipo = 7
ElseIf Index = 2 Then
	ii_Tipo = 9
Else
	ii_Tipo = -1
End If
end event

type st_7 from statictext within w_info_ventas_por_productor
integer x = 329
integer y = 964
integer width = 411
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
string text = "Nro. Documento"
boolean focusrectangle = false
end type

