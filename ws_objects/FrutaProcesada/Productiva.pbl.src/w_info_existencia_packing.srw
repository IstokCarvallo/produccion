$PBExportHeader$w_info_existencia_packing.srw
$PBExportComments$Informe Existencia Sitio Revisión
forward
global type w_info_existencia_packing from w_para_informes
end type
type st_1 from statictext within w_info_existencia_packing
end type
type st_2 from statictext within w_info_existencia_packing
end type
type em_desde from editmask within w_info_existencia_packing
end type
type st_6 from statictext within w_info_existencia_packing
end type
type st_3 from statictext within w_info_existencia_packing
end type
type st_7 from statictext within w_info_existencia_packing
end type
type em_hasta from editmask within w_info_existencia_packing
end type
type st_8 from statictext within w_info_existencia_packing
end type
type st_variedad from statictext within w_info_existencia_packing
end type
type st_embalaje from statictext within w_info_existencia_packing
end type
type cbx_embalaje from checkbox within w_info_existencia_packing
end type
type cbx_consembalaje from checkbox within w_info_existencia_packing
end type
type st_calidad from statictext within w_info_existencia_packing
end type
type em_calidad from editmask within w_info_existencia_packing
end type
type cbx_calidad from checkbox within w_info_existencia_packing
end type
type cbx_conscalidad from checkbox within w_info_existencia_packing
end type
type dw_envase from datawindow within w_info_existencia_packing
end type
type st_envase from statictext within w_info_existencia_packing
end type
type cbx_envase from checkbox within w_info_existencia_packing
end type
type dw_embalaje from datawindow within w_info_existencia_packing
end type
type st_5 from statictext within w_info_existencia_packing
end type
type cbx_cajas from checkbox within w_info_existencia_packing
end type
type gb_3 from groupbox within w_info_existencia_packing
end type
type st_4 from statictext within w_info_existencia_packing
end type
type st_9 from statictext within w_info_existencia_packing
end type
type st_10 from statictext within w_info_existencia_packing
end type
type uo_selespecie from uo_seleccion_especie within w_info_existencia_packing
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_packing
end type
type cbx_varirotula from checkbox within w_info_existencia_packing
end type
type st_11 from statictext within w_info_existencia_packing
end type
type st_12 from statictext within w_info_existencia_packing
end type
type em_numero from editmask within w_info_existencia_packing
end type
type cb_buscarepa from commandbutton within w_info_existencia_packing
end type
type cbx_todpal from checkbox within w_info_existencia_packing
end type
type st_13 from statictext within w_info_existencia_packing
end type
type cbx_columna from checkbox within w_info_existencia_packing
end type
type cbx_orden from checkbox within w_info_existencia_packing
end type
type st_14 from statictext within w_info_existencia_packing
end type
type uo_selcategoria from uo_seleccion_categoria within w_info_existencia_packing
end type
type st_15 from statictext within w_info_existencia_packing
end type
type st_16 from statictext within w_info_existencia_packing
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_existencia_packing
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_existencia_packing
end type
type uo_selplanta from uo_seleccion_plantas within w_info_existencia_packing
end type
type uo_seletiquetas from uo_seleccion_etiquetas within w_info_existencia_packing
end type
type uo_selpacking from uo_seleccion_plantas within w_info_existencia_packing
end type
end forward

global type w_info_existencia_packing from w_para_informes
integer x = 14
integer y = 32
integer width = 3909
integer height = 2116
string title = "EXISTENCIA PACKING"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
em_desde em_desde
st_6 st_6
st_3 st_3
st_7 st_7
em_hasta em_hasta
st_8 st_8
st_variedad st_variedad
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
cbx_consembalaje cbx_consembalaje
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
dw_envase dw_envase
st_envase st_envase
cbx_envase cbx_envase
dw_embalaje dw_embalaje
st_5 st_5
cbx_cajas cbx_cajas
gb_3 gb_3
st_4 st_4
st_9 st_9
st_10 st_10
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
st_11 st_11
st_12 st_12
em_numero em_numero
cb_buscarepa cb_buscarepa
cbx_todpal cbx_todpal
st_13 st_13
cbx_columna cbx_columna
cbx_orden cbx_orden
st_14 st_14
uo_selcategoria uo_selcategoria
st_15 st_15
st_16 st_16
uo_selproductor uo_selproductor
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_seletiquetas uo_seletiquetas
uo_selpacking uo_selpacking
end type
global w_info_existencia_packing w_info_existencia_packing

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_envases
						
String 				is_embalaje, is_calibre
Integer 				ii_envate, ii_envaec
Date 					id_fecini, id_fecter	

uo_calibre			iuo_calibre
end variables

on w_info_existencia_packing.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.st_6=create st_6
this.st_3=create st_3
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.st_variedad=create st_variedad
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.cbx_consembalaje=create cbx_consembalaje
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.dw_envase=create dw_envase
this.st_envase=create st_envase
this.cbx_envase=create cbx_envase
this.dw_embalaje=create dw_embalaje
this.st_5=create st_5
this.cbx_cajas=create cbx_cajas
this.gb_3=create gb_3
this.st_4=create st_4
this.st_9=create st_9
this.st_10=create st_10
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.st_11=create st_11
this.st_12=create st_12
this.em_numero=create em_numero
this.cb_buscarepa=create cb_buscarepa
this.cbx_todpal=create cbx_todpal
this.st_13=create st_13
this.cbx_columna=create cbx_columna
this.cbx_orden=create cbx_orden
this.st_14=create st_14
this.uo_selcategoria=create uo_selcategoria
this.st_15=create st_15
this.st_16=create st_16
this.uo_selproductor=create uo_selproductor
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_seletiquetas=create uo_seletiquetas
this.uo_selpacking=create uo_selpacking
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_7
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.st_8
this.Control[iCurrent+9]=this.st_variedad
this.Control[iCurrent+10]=this.st_embalaje
this.Control[iCurrent+11]=this.cbx_embalaje
this.Control[iCurrent+12]=this.cbx_consembalaje
this.Control[iCurrent+13]=this.st_calidad
this.Control[iCurrent+14]=this.em_calidad
this.Control[iCurrent+15]=this.cbx_calidad
this.Control[iCurrent+16]=this.cbx_conscalidad
this.Control[iCurrent+17]=this.dw_envase
this.Control[iCurrent+18]=this.st_envase
this.Control[iCurrent+19]=this.cbx_envase
this.Control[iCurrent+20]=this.dw_embalaje
this.Control[iCurrent+21]=this.st_5
this.Control[iCurrent+22]=this.cbx_cajas
this.Control[iCurrent+23]=this.gb_3
this.Control[iCurrent+24]=this.st_4
this.Control[iCurrent+25]=this.st_9
this.Control[iCurrent+26]=this.st_10
this.Control[iCurrent+27]=this.uo_selespecie
this.Control[iCurrent+28]=this.uo_selvariedad
this.Control[iCurrent+29]=this.cbx_varirotula
this.Control[iCurrent+30]=this.st_11
this.Control[iCurrent+31]=this.st_12
this.Control[iCurrent+32]=this.em_numero
this.Control[iCurrent+33]=this.cb_buscarepa
this.Control[iCurrent+34]=this.cbx_todpal
this.Control[iCurrent+35]=this.st_13
this.Control[iCurrent+36]=this.cbx_columna
this.Control[iCurrent+37]=this.cbx_orden
this.Control[iCurrent+38]=this.st_14
this.Control[iCurrent+39]=this.uo_selcategoria
this.Control[iCurrent+40]=this.st_15
this.Control[iCurrent+41]=this.st_16
this.Control[iCurrent+42]=this.uo_selproductor
this.Control[iCurrent+43]=this.uo_selcliente
this.Control[iCurrent+44]=this.uo_selplanta
this.Control[iCurrent+45]=this.uo_seletiquetas
this.Control[iCurrent+46]=this.uo_selpacking
end on

on w_info_existencia_packing.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.st_variedad)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.cbx_consembalaje)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
destroy(this.dw_envase)
destroy(this.st_envase)
destroy(this.cbx_envase)
destroy(this.dw_embalaje)
destroy(this.st_5)
destroy(this.cbx_cajas)
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.st_11)
destroy(this.st_12)
destroy(this.em_numero)
destroy(this.cb_buscarepa)
destroy(this.cbx_todpal)
destroy(this.st_13)
destroy(this.cbx_columna)
destroy(this.cbx_orden)
destroy(this.st_14)
destroy(this.uo_selcategoria)
destroy(this.st_15)
destroy(this.st_16)
destroy(this.uo_selproductor)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_seletiquetas)
destroy(this.uo_selpacking)
end on

event open;call super::open;Boolean lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCategoria.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
IF IsNull(uo_SelProductor.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelEtiquetas.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelPacking.Codigo) THEN lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelCategoria.Seleccion(True, True)
	uo_SelProductor.Seleccion(True,True)
	uo_SelEspecie.Seleccion(True,True)
	uo_SelVariedad.Seleccion(True,True)
	uo_SelEtiquetas.Seleccion(True,True)
	uo_SelPacking.Seleccion(False, False)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPacking.Filtra(2)
	uo_SelPacking.Inicia(gi_Packing)

	iuo_calibre   				=	Create uo_calibre
	
	dw_envase.enabled 											=	False
	dw_envase.Object.enva_codigo.BackGround.Color		=	RGB(166,180,210)
	
	dw_embalaje.enabled 											=	False
	dw_embalaje.Object.emba_codigo.BackGround.Color		=	RGB(166,180,210)
	
	dw_envase.getChild("enva_codigo", idwc_envases)
	idwc_envases.SetTransObject(SQLCA)
	idwc_envases.Retrieve()
	dw_envase.InsertRow(0)
	
	dw_embalaje.Enabled		=	False
	dw_embalaje.SetTransObject(SQLCA)
	dw_embalaje.Retrieve()
	dw_embalaje.InsertRow(0)
	
	em_desde.Text				=	String(RelativeDate(Today(), -365))
	em_hasta.Text				=	String(Today())
	
	ii_envaec					=	-1
	id_fecini					=	RelativeDate(Today(), -365)
	id_fecter					=	Today()
	is_embalaje				=	'-1'
	is_calibre				=	'-1'
	
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_existencia_packing
integer x = 3483
integer y = 820
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_packing
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_packing
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_packing
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_packing
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_packing
integer width = 3159
string text = "Existencia Packing"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_packing
integer x = 3479
integer y = 1148
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_Caja=1, li_varirotula, li_Orden=1
String		texto_desde, texto_hasta, texto_fecha
Long		ll_pallet

istr_info.titulo	= 'EXISTENCIA EN PACKING'

OpenWithParm(vinf, istr_info)

If cbx_columna.Checked Then
	vinf.dw_1.DataObject = "dw_info_existencia_packing_columna"
Else
	vinf.dw_1.DataObject = "dw_info_existencia_packing"
End If

texto_desde	=  f_fecha_texto(String(id_fecini), 1)
texto_hasta	=	f_fecha_texto(String(id_fecter), 1)
texto_fecha	=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

vinf.dw_1.SetTransObject(sqlca)

If IsNull(ii_envate) OR IsNull(ii_envaec) OR IsNull(is_calibre) OR IsNull(id_fecini) &
	OR IsNull(id_fecter)Then
		MessageBox("Error", "Debe Ingresar todos los parametros para el informe", StopSign!)
		return 1
End If

If ii_envate = 0 Then
	ii_envate 	= -1
End If
	
If cbx_cajas.Checked = True Then
	li_Caja 	= 2
Else
	li_Caja 	= 1
End If	

If cbx_orden.Checked = True Then
	li_Orden = 2
Else
	li_Orden = 1
End If	

If cbx_varirotula.Checked Then
	li_varirotula = 1
Else
	li_varirotula = 0
End If

If cbx_todpal.Checked Then
	ll_pallet = -1
Else
	ll_pallet = Long(em_numero.Text)
End If	

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo,uo_SelVariedad.Codigo, &
								ii_envate,ii_envaec,is_embalaje,is_calibre,id_fecini,id_fecter, uo_SelPacking.Codigo, li_caja, &
								li_varirotula,ll_pallet,li_Orden,   uo_SelProductor.Lista,  uo_SelCategoria.Codigo,uo_SelEtiquetas.Codigo)
If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("fechas.text = '" + texto_fecha + "'")
	vinf.dw_1.ModIfy("t_cliente.text = '" + uo_SelCliente.Nombre + "'")		
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_packing
integer x = 3479
integer y = 1416
integer taborder = 140
end type

type st_1 from statictext within w_info_existencia_packing
integer x = 293
integer y = 656
integer width = 238
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_existencia_packing
integer x = 343
integer y = 1292
integer width = 229
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_existencia_packing
integer x = 617
integer y = 1276
integer width = 430
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[9]	=	This.Text
id_fecini					= 	Date(this.Text)
end event

type st_6 from statictext within w_info_existencia_packing
integer x = 293
integer y = 476
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_existencia_packing
integer x = 293
integer y = 1104
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_existencia_packing
integer x = 1083
integer y = 1292
integer width = 197
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_existencia_packing
integer x = 1307
integer y = 1276
integer width = 430
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[10]	=	This.Text
id_fecter					= 	Date(this.Text)
end event

type st_8 from statictext within w_info_existencia_packing
integer x = 293
integer y = 936
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_existencia_packing
integer x = 1851
integer y = 548
integer width = 302
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_embalaje from statictext within w_info_existencia_packing
integer x = 1851
integer y = 932
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_existencia_packing
integer x = 2194
integer y = 840
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consembalaje.Enabled	=	True
	dw_embalaje.Enabled			=	False
	istr_mant.argumento[6]		=	'Z'
	istr_mant.argumento[16]		=	'0'
	dw_embalaje.Object.emba_codigo.BackGround.Color	=	RGB(166,180,210)
	is_embalaje						= '-1'
ELSE
	cbx_consembalaje.Enabled	=	False
	cbx_consembalaje.Checked	=	False
	dw_embalaje.Enabled			=	True
	dw_embalaje.Object.emba_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_embalaje.SetFocus()
	SetNull(is_embalaje)
END IF
end event

type cbx_consembalaje from checkbox within w_info_existencia_packing
integer x = 2711
integer y = 840
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'1'
	is_embalaje					= '-9'
ELSE
	istr_mant.argumento[16]	=	'0'
	is_embalaje					= '-1'
END IF

end event

type st_calidad from statictext within w_info_existencia_packing
integer x = 1851
integer y = 1120
integer width = 256
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Calidad"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_existencia_packing
integer x = 2194
integer y = 1108
integer width = 297
integer height = 96
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;If This.Text <> '' Then
	IF NOT iuo_Calibre.Existe(uo_SelEspecie.Codigo,uo_SelVariedad.Codigo,This.Text, True, SQLCA) Then
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	Else
		istr_mant.argumento[8]	=	iuo_Calibre.Calibre
		em_calidad.Text			=	iuo_Calibre.Calibre
		is_calibre 					=  iuo_Calibre.Calibre
		Return 1
	End If
End If


end event

type cbx_calidad from checkbox within w_info_existencia_packing
integer x = 2194
integer y = 1024
integer width = 297
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_conscalidad.Enabled	=	True
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[8]		=	'Z'
	istr_mant.argumento[18]		=	'0'
	is_calibre						= '-1'
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
	SetNull(is_calibre)
END IF


end event

type cbx_conscalidad from checkbox within w_info_existencia_packing
integer x = 2711
integer y = 1024
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidadas"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[18]	=	'1'
	is_calibre 					= '-9'
ELSE
	istr_mant.argumento[18]	=	'0'
	is_calibre 					= '-1'
END IF

end event

type dw_envase from datawindow within w_info_existencia_packing
integer x = 2190
integer y = 716
integer width = 1138
integer height = 104
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_envases_1"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String					ls_Columna[], ls_null, ls_filtro
DataWindowChild 	ldwc_Embalajes
SetNull(ls_null)


ii_EnvaTE	= 	Integer(left(data, 2))
ii_EnvaEC	= 	Integer(mid(data, 6,2))

dw_embalaje.Reset()
dw_embalaje.GetChild("emba_codigo", ldwc_Embalajes)
ldwc_Embalajes.SetTransObject(SQLCA)
ldwc_Embalajes.Retrieve(-1)
ls_filtro = "enva_tipoen = " + String(ii_envate) + " and enva_codigo = " + String(ii_envaec)
ldwc_embalajes.SetFilter(ls_filtro)
ldwc_embalajes.Filter()
dw_embalaje.InsertRow(0)
end event

event itemerror;RETURN 1
end event

type st_envase from statictext within w_info_existencia_packing
integer x = 1851
integer y = 748
integer width = 247
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Envases"
boolean focusrectangle = false
end type

type cbx_envase from checkbox within w_info_existencia_packing
integer x = 2194
integer y = 648
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_envase.Enabled			=	False
	dw_envase.Object.enva_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[6]		=	'Z'
	istr_mant.argumento[16]		=	'0'
	ii_envate						= 	-1
	ii_envaec						= 	-1
ELSE
	dw_envase.Enabled			=	True
	dw_envase.Object.enva_codigo.BackGround.Color	=	RGB(255, 255, 255)
	SetNull(ii_envate)
	SetNull(ii_envaec)
END IF
end event

type dw_embalaje from datawindow within w_info_existencia_packing
integer x = 2194
integer y = 916
integer width = 1143
integer height = 104
integer taborder = 60
boolean bringtotop = true
string dataobject = "dddw_embalajes"
boolean border = false
boolean livescroll = true
end type

event itemchanged;is_embalaje 				=	data
end event

event itemerror;RETURN 1
end event

type st_5 from statictext within w_info_existencia_packing
integer x = 1851
integer y = 1248
integer width = 238
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Packing"
alignment alignment = right!
boolean focusrectangle = false
end type

type cbx_cajas from checkbox within w_info_existencia_packing
integer x = 2103
integer y = 1632
integer width = 585
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Abre por Nº Caja"
end type

type gb_3 from groupbox within w_info_existencia_packing
integer x = 288
integer y = 1204
integer width = 1490
integer height = 200
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Embalaje"
end type

type st_4 from statictext within w_info_existencia_packing
integer x = 251
integer y = 444
integer width = 1573
integer height = 1140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_existencia_packing
integer x = 1819
integer y = 444
integer width = 1586
integer height = 1140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_existencia_packing
integer x = 1326
integer y = 1584
integer width = 1381
integer height = 164
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_existencia_packing
event destroy ( )
integer x = 654
integer y = 1028
integer height = 180
integer taborder = 80
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled	=	True
		uo_selvariedad.dw_Seleccion.Enabled		=	False
		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
		uo_selvariedad.dw_Seleccion.Enabled		=	True
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_packing
event destroy ( )
integer x = 2181
integer y = 460
integer taborder = 90
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_existencia_packing
integer x = 2743
integer y = 1632
integer width = 645
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rotulada"
end type

type st_11 from statictext within w_info_existencia_packing
integer x = 251
integer y = 1584
integer width = 1074
integer height = 164
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_existencia_packing
integer x = 293
integer y = 1632
integer width = 517
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_existencia_packing
integer x = 475
integer y = 1620
integer width = 466
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[2]	=	This.Text

end event

type cb_buscarepa from commandbutton within w_info_existencia_packing
integer x = 946
integer y = 1624
integer width = 91
integer height = 84
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;istr_busq.argum[1]	=	istr_mant.argumento[1]
istr_busq.argum[5]   = ''

OpenWithParm(w_busc_palletencab, istr_busq)

istr_busq = Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	istr_mant.argumento[2] 	=	istr_busq.argum[2]
	em_numero.Text 			=	istr_busq.argum[2]
END IF
end event

type cbx_todpal from checkbox within w_info_existencia_packing
integer x = 1042
integer y = 1632
integer width = 279
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_numero.Text = ''
	em_numero.Enabled = False
	cb_buscarepa.Enabled = False
ELSE	
	em_numero.Enabled = True
	cb_buscarepa.Enabled = True
END IF
end event

type st_13 from statictext within w_info_existencia_packing
integer x = 247
integer y = 1748
integer width = 3159
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_columna from checkbox within w_info_existencia_packing
integer x = 1467
integer y = 1772
integer width = 713
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Informe en Columna  "
end type

type cbx_orden from checkbox within w_info_existencia_packing
integer x = 1339
integer y = 1632
integer width = 754
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Abre por O.de Proceso"
end type

type st_14 from statictext within w_info_existencia_packing
integer x = 2706
integer y = 1584
integer width = 699
integer height = 164
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcategoria from uo_seleccion_categoria within w_info_existencia_packing
integer x = 640
integer y = 1404
integer taborder = 340
boolean bringtotop = true
end type

on uo_selcategoria.destroy
call uo_seleccion_categoria::destroy
end on

type st_15 from statictext within w_info_existencia_packing
integer x = 293
integer y = 1484
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Categoria"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_16 from statictext within w_info_existencia_packing
integer x = 1851
integer y = 1432
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Etiqueta"
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_existencia_packing
integer x = 654
integer y = 748
integer taborder = 130
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_existencia_packing
event destroy ( )
integer x = 654
integer y = 468
integer height = 84
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelProductor.Filtra(-1,-1, -1)
		
	Case Else
		uo_SelProductor.Filtra(-1,-1, This.Codigo)
		
End Choose
end event

type uo_selplanta from uo_seleccion_plantas within w_info_existencia_packing
event destroy ( )
integer x = 654
integer y = 560
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_seletiquetas from uo_seleccion_etiquetas within w_info_existencia_packing
integer x = 2194
integer y = 1336
integer taborder = 90
boolean bringtotop = true
end type

on uo_seletiquetas.destroy
call uo_seleccion_etiquetas::destroy
end on

type uo_selpacking from uo_seleccion_plantas within w_info_existencia_packing
integer x = 2194
integer y = 1240
integer height = 84
integer taborder = 90
boolean bringtotop = true
end type

on uo_selpacking.destroy
call uo_seleccion_plantas::destroy
end on

