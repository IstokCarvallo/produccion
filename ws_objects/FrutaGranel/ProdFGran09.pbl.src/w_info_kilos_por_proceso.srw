$PBExportHeader$w_info_kilos_por_proceso.srw
forward
global type w_info_kilos_por_proceso from w_para_informes
end type
type st_6 from statictext within w_info_kilos_por_proceso
end type
type st_1 from statictext within w_info_kilos_por_proceso
end type
type st_8 from statictext within w_info_kilos_por_proceso
end type
type st_3 from statictext within w_info_kilos_por_proceso
end type
type st_variedad from statictext within w_info_kilos_por_proceso
end type
type st_11 from statictext within w_info_kilos_por_proceso
end type
type st_embalaje from statictext within w_info_kilos_por_proceso
end type
type st_calidad from statictext within w_info_kilos_por_proceso
end type
type st_5 from statictext within w_info_kilos_por_proceso
end type
type st_4 from statictext within w_info_kilos_por_proceso
end type
type uo_selespecie from uo_seleccion_especie within w_info_kilos_por_proceso
end type
type uo_selplanta from uo_seleccion_plantas within w_info_kilos_por_proceso
end type
type uo_selproductor from uo_seleccion_productor within w_info_kilos_por_proceso
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_kilos_por_proceso
end type
type cbx_etiqueta from checkbox within w_info_kilos_por_proceso
end type
type cbx_consetiqueta from checkbox within w_info_kilos_por_proceso
end type
type dw_etiqueta from datawindow within w_info_kilos_por_proceso
end type
type cbx_embalaje from checkbox within w_info_kilos_por_proceso
end type
type em_embalaje from editmask within w_info_kilos_por_proceso
end type
type cbx_consembalaje from checkbox within w_info_kilos_por_proceso
end type
type cbx_conscalidad from checkbox within w_info_kilos_por_proceso
end type
type cbx_calidad from checkbox within w_info_kilos_por_proceso
end type
type em_calidad from editmask within w_info_kilos_por_proceso
end type
type cb_buscaembalaje from commandbutton within w_info_kilos_por_proceso
end type
type st_2 from statictext within w_info_kilos_por_proceso
end type
type em_desde from editmask within w_info_kilos_por_proceso
end type
type st_7 from statictext within w_info_kilos_por_proceso
end type
type em_hasta from editmask within w_info_kilos_por_proceso
end type
type cbx_fecemb from checkbox within w_info_kilos_por_proceso
end type
type em_orden from editmask within w_info_kilos_por_proceso
end type
type cbx_ordentodo from checkbox within w_info_kilos_por_proceso
end type
type st_17 from statictext within w_info_kilos_por_proceso
end type
type cbx_todosfecha from checkbox within w_info_kilos_por_proceso
end type
type r_1 from rectangle within w_info_kilos_por_proceso
end type
type gb_4 from groupbox within w_info_kilos_por_proceso
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_kilos_por_proceso
end type
type cbx_consproc from checkbox within w_info_kilos_por_proceso
end type
type cbx_exporta from checkbox within w_info_kilos_por_proceso
end type
type sle_estado from singlelineedit within w_info_kilos_por_proceso
end type
type ddlb_tipo from dropdownlistbox within w_info_kilos_por_proceso
end type
type st_9 from statictext within w_info_kilos_por_proceso
end type
type st_12 from statictext within w_info_kilos_por_proceso
end type
type gb_5 from groupbox within w_info_kilos_por_proceso
end type
type gb_6 from groupbox within w_info_kilos_por_proceso
end type
type gb_3 from groupbox within w_info_kilos_por_proceso
end type
type st_10 from statictext within w_info_kilos_por_proceso
end type
type em_pallet from editmask within w_info_kilos_por_proceso
end type
type cbx_todospallet from checkbox within w_info_kilos_por_proceso
end type
type st_13 from statictext within w_info_kilos_por_proceso
end type
type rb_1 from radiobutton within w_info_kilos_por_proceso
end type
type rb_2 from radiobutton within w_info_kilos_por_proceso
end type
type cbx_conscajas from checkbox within w_info_kilos_por_proceso
end type
type dw_1 from datawindow within w_info_kilos_por_proceso
end type
end forward

global type w_info_kilos_por_proceso from w_para_informes
integer width = 3726
integer height = 1672
string title = "Informe de Kilos Por Proceso"
boolean minbox = false
boolean maxbox = false
st_6 st_6
st_1 st_1
st_8 st_8
st_3 st_3
st_variedad st_variedad
st_11 st_11
st_embalaje st_embalaje
st_calidad st_calidad
st_5 st_5
st_4 st_4
uo_selespecie uo_selespecie
uo_selplanta uo_selplanta
uo_selproductor uo_selproductor
uo_selvariedad uo_selvariedad
cbx_etiqueta cbx_etiqueta
cbx_consetiqueta cbx_consetiqueta
dw_etiqueta dw_etiqueta
cbx_embalaje cbx_embalaje
em_embalaje em_embalaje
cbx_consembalaje cbx_consembalaje
cbx_conscalidad cbx_conscalidad
cbx_calidad cbx_calidad
em_calidad em_calidad
cb_buscaembalaje cb_buscaembalaje
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
cbx_fecemb cbx_fecemb
em_orden em_orden
cbx_ordentodo cbx_ordentodo
st_17 st_17
cbx_todosfecha cbx_todosfecha
r_1 r_1
gb_4 gb_4
uo_selcliente uo_selcliente
cbx_consproc cbx_consproc
cbx_exporta cbx_exporta
sle_estado sle_estado
ddlb_tipo ddlb_tipo
st_9 st_9
st_12 st_12
gb_5 gb_5
gb_6 gb_6
gb_3 gb_3
st_10 st_10
em_pallet em_pallet
cbx_todospallet cbx_todospallet
st_13 st_13
rb_1 rb_1
rb_2 rb_2
cbx_conscajas cbx_conscajas
dw_1 dw_1
end type
global w_info_kilos_por_proceso w_info_kilos_por_proceso

type variables
str_mant				istr_mant

Integer				ii_nroorden, ii_tipoorden, ii_cliente, ii_informe

DataWindowChild	idwc_etiqueta
end variables

forward prototypes
public function boolean noexisteetiqueta (integer li_etiqueta)
end prototypes

public function boolean noexisteetiqueta (integer li_etiqueta);String	ls_nombre


SELECT	etiq_nombre
INTO    :ls_nombre
FROM	dbo.etiquetas
WHERE	etiq_codigo =  :li_etiqueta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Etiquetas")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Etiqueta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	RETURN False 
END IF
end function

on w_info_kilos_por_proceso.create
int iCurrent
call super::create
this.st_6=create st_6
this.st_1=create st_1
this.st_8=create st_8
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_11=create st_11
this.st_embalaje=create st_embalaje
this.st_calidad=create st_calidad
this.st_5=create st_5
this.st_4=create st_4
this.uo_selespecie=create uo_selespecie
this.uo_selplanta=create uo_selplanta
this.uo_selproductor=create uo_selproductor
this.uo_selvariedad=create uo_selvariedad
this.cbx_etiqueta=create cbx_etiqueta
this.cbx_consetiqueta=create cbx_consetiqueta
this.dw_etiqueta=create dw_etiqueta
this.cbx_embalaje=create cbx_embalaje
this.em_embalaje=create em_embalaje
this.cbx_consembalaje=create cbx_consembalaje
this.cbx_conscalidad=create cbx_conscalidad
this.cbx_calidad=create cbx_calidad
this.em_calidad=create em_calidad
this.cb_buscaembalaje=create cb_buscaembalaje
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.cbx_fecemb=create cbx_fecemb
this.em_orden=create em_orden
this.cbx_ordentodo=create cbx_ordentodo
this.st_17=create st_17
this.cbx_todosfecha=create cbx_todosfecha
this.r_1=create r_1
this.gb_4=create gb_4
this.uo_selcliente=create uo_selcliente
this.cbx_consproc=create cbx_consproc
this.cbx_exporta=create cbx_exporta
this.sle_estado=create sle_estado
this.ddlb_tipo=create ddlb_tipo
this.st_9=create st_9
this.st_12=create st_12
this.gb_5=create gb_5
this.gb_6=create gb_6
this.gb_3=create gb_3
this.st_10=create st_10
this.em_pallet=create em_pallet
this.cbx_todospallet=create cbx_todospallet
this.st_13=create st_13
this.rb_1=create rb_1
this.rb_2=create rb_2
this.cbx_conscajas=create cbx_conscajas
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_6
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_8
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_variedad
this.Control[iCurrent+6]=this.st_11
this.Control[iCurrent+7]=this.st_embalaje
this.Control[iCurrent+8]=this.st_calidad
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.uo_selespecie
this.Control[iCurrent+12]=this.uo_selplanta
this.Control[iCurrent+13]=this.uo_selproductor
this.Control[iCurrent+14]=this.uo_selvariedad
this.Control[iCurrent+15]=this.cbx_etiqueta
this.Control[iCurrent+16]=this.cbx_consetiqueta
this.Control[iCurrent+17]=this.dw_etiqueta
this.Control[iCurrent+18]=this.cbx_embalaje
this.Control[iCurrent+19]=this.em_embalaje
this.Control[iCurrent+20]=this.cbx_consembalaje
this.Control[iCurrent+21]=this.cbx_conscalidad
this.Control[iCurrent+22]=this.cbx_calidad
this.Control[iCurrent+23]=this.em_calidad
this.Control[iCurrent+24]=this.cb_buscaembalaje
this.Control[iCurrent+25]=this.st_2
this.Control[iCurrent+26]=this.em_desde
this.Control[iCurrent+27]=this.st_7
this.Control[iCurrent+28]=this.em_hasta
this.Control[iCurrent+29]=this.cbx_fecemb
this.Control[iCurrent+30]=this.em_orden
this.Control[iCurrent+31]=this.cbx_ordentodo
this.Control[iCurrent+32]=this.st_17
this.Control[iCurrent+33]=this.cbx_todosfecha
this.Control[iCurrent+34]=this.r_1
this.Control[iCurrent+35]=this.gb_4
this.Control[iCurrent+36]=this.uo_selcliente
this.Control[iCurrent+37]=this.cbx_consproc
this.Control[iCurrent+38]=this.cbx_exporta
this.Control[iCurrent+39]=this.sle_estado
this.Control[iCurrent+40]=this.ddlb_tipo
this.Control[iCurrent+41]=this.st_9
this.Control[iCurrent+42]=this.st_12
this.Control[iCurrent+43]=this.gb_5
this.Control[iCurrent+44]=this.gb_6
this.Control[iCurrent+45]=this.gb_3
this.Control[iCurrent+46]=this.st_10
this.Control[iCurrent+47]=this.em_pallet
this.Control[iCurrent+48]=this.cbx_todospallet
this.Control[iCurrent+49]=this.st_13
this.Control[iCurrent+50]=this.rb_1
this.Control[iCurrent+51]=this.rb_2
this.Control[iCurrent+52]=this.cbx_conscajas
this.Control[iCurrent+53]=this.dw_1
end on

on w_info_kilos_por_proceso.destroy
call super::destroy
destroy(this.st_6)
destroy(this.st_1)
destroy(this.st_8)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_11)
destroy(this.st_embalaje)
destroy(this.st_calidad)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.uo_selespecie)
destroy(this.uo_selplanta)
destroy(this.uo_selproductor)
destroy(this.uo_selvariedad)
destroy(this.cbx_etiqueta)
destroy(this.cbx_consetiqueta)
destroy(this.dw_etiqueta)
destroy(this.cbx_embalaje)
destroy(this.em_embalaje)
destroy(this.cbx_consembalaje)
destroy(this.cbx_conscalidad)
destroy(this.cbx_calidad)
destroy(this.em_calidad)
destroy(this.cb_buscaembalaje)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.cbx_fecemb)
destroy(this.em_orden)
destroy(this.cbx_ordentodo)
destroy(this.st_17)
destroy(this.cbx_todosfecha)
destroy(this.r_1)
destroy(this.gb_4)
destroy(this.uo_selcliente)
destroy(this.cbx_consproc)
destroy(this.cbx_exporta)
destroy(this.sle_estado)
destroy(this.ddlb_tipo)
destroy(this.st_9)
destroy(this.st_12)
destroy(this.gb_5)
destroy(this.gb_6)
destroy(this.gb_3)
destroy(this.st_10)
destroy(this.em_pallet)
destroy(this.cbx_todospallet)
destroy(this.st_13)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.cbx_conscajas)
destroy(this.dw_1)
end on

event open;call super::open;Boolean lb_cerrar

IF IsNull(uo_SelCliente.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo)THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar	=	True

ii_informe 	= Integer(Message.StringParm)

uo_SelCliente.cbx_consolida.Visible 	= 	False
uo_SelCliente.cbx_Todos.Checked			=	False
uo_SelPlanta.cbx_consolida.Visible 		= 	False
uo_SelProductor.cbx_consolida.Visible 	= 	False
uo_SelEspecie.cbx_consolida.Visible 	= 	False
uo_SelVariedad.cbx_consolida.Visible 	= 	False

uo_SelCliente.cbx_Todos.TriggerEvent("clicked")

IF lb_Cerrar THEN
	Close(This)
END IF

dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()
dw_etiqueta.InsertRow(0)

cbx_todosfecha.TriggerEvent("clicked")

em_desde.Text 											= 	String(RelativeDate(Today(), -365))
em_hasta.Text 											= 	String(Today())
ii_tipoorden 											=	-1
uo_SelCliente.Codigo 								= 	gi_CodExport
uo_SelCliente.dw_seleccion.Object.Codigo[1]	= 	gi_CodExport

ddlb_tipo.selectItem("Todos", 4)	
cbx_todospallet.Enabled = False
cbx_todospallet.Checked = True

cbx_todospallet.TriggerEvent("clicked")

IF Upper(gstr_us.nombre) <> Upper(gstr_paramplanta.jefeplanta) OR IsNull(gstr_paramplanta.jefeplanta) THEN
	MessageBox("Advertencia", "Esta ventana no puede ser accesada por el usuario " + gstr_us.nombre, StopSign!)
	Close(This)
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_kilos_por_proceso
integer x = 3337
integer y = 588
end type

type st_computador from w_para_informes`st_computador within w_info_kilos_por_proceso
end type

type st_usuario from w_para_informes`st_usuario within w_info_kilos_por_proceso
end type

type st_temporada from w_para_informes`st_temporada within w_info_kilos_por_proceso
end type

type p_logo from w_para_informes`p_logo within w_info_kilos_por_proceso
end type

type st_titulo from w_para_informes`st_titulo within w_info_kilos_por_proceso
integer width = 3013
string text = "Informe de Kilos Por Proceso"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_kilos_por_proceso
integer x = 3355
integer y = 968
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Integer		li_etiqueta, 	li_consfecha, 	li_conscajas
String		ls_embalaje, 	ls_calidad, 	ls_Archivo,	ls_ruta
Long			ll_nroorden, 	fila, 			ll_pallet
Date			ld_desde, 		ld_hasta

SetPointer(HourGlass!)

istr_info.titulo	= "INFORME DE KILOS POR PROCESO"
istr_info.copias	= 1

IF cbx_ordentodo.Checked THEN
	ll_nroOrden	= 	-1
	IF cbx_consproc.Checked THEN	ll_nroOrden	= 	-9
ELSE
	ll_nroOrden 	= 	Long(em_orden.Text)
END IF

IF cbx_todospallet.Checked	THEN
	ll_pallet	=	-1
ELSE
	ll_pallet	=	Long(em_pallet.Text)
END IF

IF cbx_conscajas.Checked THEN	li_conscajas	= 	-9

IF cbx_embalaje.Checked THEN
	ls_embalaje 	= '-1'
	IF cbx_consembalaje.Checked THEN	ls_embalaje 	= '-9'
ELSE
	ls_embalaje 	= 	em_embalaje.Text
END IF

IF cbx_calidad.Checked THEN
	ls_calidad 	= '-1'
	IF cbx_conscalidad.Checked THEN	ls_calidad 	= '-9'
ELSE
	ls_calidad 	= 	em_calidad.Text
END IF

IF cbx_etiqueta.Checked THEN
	li_etiqueta 	= -1
	IF cbx_consetiqueta.Checked THEN	li_etiqueta 	= -9
ELSE
	li_etiqueta 	= 	dw_etiqueta.object.etiq_codigo[1]
END IF

IF cbx_fecemb.Checked THEN li_ConsFecha = 1
ld_desde			=	Date(em_desde.Text)
ld_hasta			=	Date(em_hasta.Text)

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_kilos_por_proceso"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_Selplanta.codigo, uo_SelProductor.codigo, &
								  uo_SelEspecie.codigo, uo_SelVariedad.codigo, ld_desde, ld_hasta,   &
								  ii_tipoorden, ll_nroorden)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_kilos_por_proceso
integer x = 3355
integer y = 1224
fontcharset fontcharset = ansi!
end type

type st_6 from statictext within w_info_kilos_por_proceso
integer x = 297
integer y = 536
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

type st_1 from statictext within w_info_kilos_por_proceso
integer x = 297
integer y = 708
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

type st_8 from statictext within w_info_kilos_por_proceso
integer x = 297
integer y = 912
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

type st_3 from statictext within w_info_kilos_por_proceso
integer x = 1874
integer y = 516
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

type st_variedad from statictext within w_info_kilos_por_proceso
integer x = 1874
integer y = 712
integer width = 302
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_kilos_por_proceso
boolean visible = false
integer x = 3808
integer y = 776
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Etiqueta"
boolean focusrectangle = false
end type

type st_embalaje from statictext within w_info_kilos_por_proceso
boolean visible = false
integer x = 3808
integer y = 980
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_calidad from statictext within w_info_kilos_por_proceso
boolean visible = false
integer x = 3813
integer y = 1180
integer width = 256
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Calibre"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_kilos_por_proceso
integer x = 1787
integer y = 428
integer width = 1467
integer height = 604
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_kilos_por_proceso
integer x = 242
integer y = 428
integer width = 1545
integer height = 604
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_kilos_por_proceso
event destroy ( )
integer x = 2235
integer y = 444
integer taborder = 20
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled		=	True
		uo_selvariedad.dw_Seleccion.Enabled		=	False		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
END CHOOSE
end event

type uo_selplanta from uo_seleccion_plantas within w_info_kilos_por_proceso
event destroy ( )
integer x = 805
integer y = 628
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_kilos_por_proceso
event destroy ( )
integer x = 805
integer y = 828
integer taborder = 60
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_kilos_por_proceso
event destroy ( )
integer x = 2235
integer y = 628
integer taborder = 50
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_etiqueta from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 4169
integer y = 696
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consetiqueta.Enabled									=	True
	dw_etiqueta.Enabled										=	False
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[7]									=	'0'
	istr_mant.argumento[17]									=	'0'
ELSE
	cbx_consetiqueta.Enabled									=	False
	cbx_consetiqueta.Checked									=	False
	dw_etiqueta.Enabled											=	True
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_etiqueta.SetFocus()
	istr_mant.argumento[17]	=	'0'
END IF
end event

type cbx_consetiqueta from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 4654
integer y = 696
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidadas"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[17]	=	'-9'
ELSE
	istr_mant.argumento[17]	=	'0'
END IF

end event

type dw_etiqueta from datawindow within w_info_kilos_por_proceso
boolean visible = false
integer x = 4155
integer y = 764
integer width = 905
integer height = 96
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_etiquetas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
IF NoExisteEtiqueta(Integer(data)) THEN
	dw_etiqueta.SetItem(1, "etiq_codigo", 1)
	dw_etiqueta.SetFocus()
	RETURN 1
ELSE
	istr_mant.argumento[7]	=	data
	
END IF


end event

event itemerror;RETURN 1
end event

type cbx_embalaje from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 4169
integer y = 896
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consembalaje.Enabled	=	True
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[6]		=	'Z'
	istr_mant.argumento[16]		=	'0'
ELSE
	cbx_consembalaje.Enabled	=	False
	cbx_consembalaje.Checked	=	False
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type em_embalaje from editmask within w_info_kilos_por_proceso
boolean visible = false
integer x = 4174
integer y = 964
integer width = 297
integer height = 92
integer taborder = 100
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
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxx"
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	uo_selcliente.Codigo // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dba.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido para el cliente " + String(li_cliente) + ".~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[6]	=	ls_embalaje
END IF
end event

type cbx_consembalaje from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 4654
integer y = 896
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidados"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'.9'
ELSE
	istr_mant.argumento[16]	=	'0'
END IF

end event

type cbx_conscalidad from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 3753
integer y = 812
integer width = 471
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidados"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[18]	=	'-9'
ELSE
	istr_mant.argumento[18]	=	'0'
END IF

end event

type cbx_calidad from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 4169
integer y = 1088
integer width = 297
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_conscalidad.Enabled	=	True
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[8]		=	'Z'
	istr_mant.argumento[18]		=	'0'
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type em_calidad from editmask within w_info_kilos_por_proceso
boolean visible = false
integer x = 4174
integer y = 1160
integer width = 297
integer height = 92
integer taborder = 110
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

event modified;Integer	li_cliente,	li_especie, li_variedad, li_cantid
String	ls_calibre

li_cliente	=	uo_selcliente.Codigo // Cliente
li_especie	=	uo_selespecie.Codigo // Especie
li_variedad	=	uo_selvariedad.Codigo // Variedad
ls_calibre	=	This.Text

ls_calibre	=	Trim(ls_calibre) + Fill(" ",3 - Len(ls_calibre))

SELECT	Count(*)
	INTO	:li_cantid
	FROM	dba.variecalibre
	WHERE	espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad
	AND	vaca_calibr	=	:ls_calibre;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variecalibre")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Calibre no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[8]	=	ls_calibre
	em_calidad.Text			=	ls_calibre
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_kilos_por_proceso
boolean visible = false
integer x = 4475
integer y = 960
integer width = 119
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

type st_2 from statictext within w_info_kilos_por_proceso
integer x = 430
integer y = 1340
integer width = 425
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
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_kilos_por_proceso
integer x = 937
integer y = 1324
integer width = 421
integer height = 96
integer taborder = 150
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
end event

type st_7 from statictext within w_info_kilos_por_proceso
integer x = 1371
integer y = 1340
integer width = 279
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
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_kilos_por_proceso
integer x = 1696
integer y = 1324
integer width = 421
integer height = 96
integer taborder = 160
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
end event

type cbx_fecemb from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 3785
integer y = 876
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidada"
end type

event clicked;IF THIS.Checked THEN
	cbx_todosfecha.Checked 	= 	True
	cbx_todosfecha.TriggerEvent("clicked")
	
	cbx_todosfecha.Enabled 	= 	False
ELSE
	cbx_todosfecha.Enabled 	= 	True
END IF
end event

type em_orden from editmask within w_info_kilos_por_proceso
integer x = 937
integer y = 1132
integer width = 375
integer height = 96
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
string minmax = "~~8"
end type

event modified;ii_nroorden	=	Long(This.Text)
end event

type cbx_ordentodo from checkbox within w_info_kilos_por_proceso
integer x = 2208
integer y = 1144
integer width = 293
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_orden.Enabled										=	False
	em_orden.Text 											=	''
	ii_nroorden													=	-1
ELSE
	em_orden.Enabled										=	True
	ii_nroorden													=	Long(em_orden.Text)
	em_orden.SetFocus()
END IF
end event

type st_17 from statictext within w_info_kilos_por_proceso
integer x = 430
integer y = 1148
integer width = 443
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
string text = "Orden Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_kilos_por_proceso
integer x = 2208
integer y = 1336
integer width = 293
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_desde.Text 		= String(RelativeDate(Today(), -365))
	em_hasta.Text 		= String(Today())
	em_desde.enabled	= False
	em_hasta.enabled		= False
ELSE
	em_desde.enabled	= True
	em_hasta.enabled		= True
END IF
end event

type r_1 from rectangle within w_info_kilos_por_proceso
integer linethickness = 4
long fillcolor = 16777215
integer x = 2569
integer y = 1184
integer width = 229
integer height = 200
end type

type gb_4 from groupbox within w_info_kilos_por_proceso
integer x = 361
integer y = 1064
integer width = 2784
integer height = 196
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_kilos_por_proceso
integer x = 805
integer y = 444
integer taborder = 60
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type cbx_consproc from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 3785
integer y = 684
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidada"
end type

event clicked;IF THIS.Checked THEN
	cbx_ordentodo.Checked 	= 	True
	cbx_ordentodo.TriggerEvent("clicked")
	
	cbx_ordentodo.Enabled 		= 	False
ELSE
	cbx_ordentodo.Enabled 		= 	True
END IF
end event

type cbx_exporta from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 4654
integer y = 1088
integer width = 507
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Exportar a Excel"
end type

event clicked;IF This.Checked THEN
	sle_estado.Visible	=	True
	sle_estado.Text		=	"En Espera"
ELSE
	sle_estado.Visible	=	True
END IF
end event

type sle_estado from singlelineedit within w_info_kilos_por_proceso
boolean visible = false
integer x = 4562
integer y = 1160
integer width = 699
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 10789024
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type ddlb_tipo from dropdownlistbox within w_info_kilos_por_proceso
integer x = 1586
integer y = 1132
integer width = 480
integer height = 400
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean sorted = false
string item[] = {"Proceso","Reproceso","Reembalaje","Todos"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE index
	CASE 1
		ii_tipoorden = 4
	CASE 2
		ii_tipoorden = 7
	CASE 3
		ii_tipoorden = 8
	CASE ELSE
		ii_tipoorden = -1
END CHOOSE
end event

type st_9 from statictext within w_info_kilos_por_proceso
integer x = 1371
integer y = 1148
integer width = 210
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo"
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_kilos_por_proceso
boolean visible = false
integer x = 3790
integer y = 1548
integer width = 425
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Nro Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type gb_5 from groupbox within w_info_kilos_por_proceso
integer x = 361
integer y = 1268
integer width = 2784
integer height = 196
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

type gb_6 from groupbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 3790
integer y = 1464
integer width = 3031
integer height = 196
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_3 from groupbox within w_info_kilos_por_proceso
integer x = 3712
integer y = 1488
integer width = 3031
integer height = 196
integer taborder = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_10 from statictext within w_info_kilos_por_proceso
integer x = 242
integer y = 1032
integer width = 3013
integer height = 484
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_pallet from editmask within w_info_kilos_por_proceso
boolean visible = false
integer x = 3790
integer y = 1532
integer width = 375
integer height = 96
integer taborder = 170
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
string minmax = "~~8"
end type

event modified;ii_nroorden	=	Long(This.Text)
end event

type cbx_todospallet from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 3790
integer y = 1544
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_pallet.Text 		= ""
	em_pallet.enabled		= False
ELSE
	em_pallet.enabled		= True
END IF
end event

type st_13 from statictext within w_info_kilos_por_proceso
boolean visible = false
integer x = 3730
integer y = 240
integer width = 3259
integer height = 148
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_info_kilos_por_proceso
boolean visible = false
integer x = 3730
integer y = 276
integer width = 603
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cajas por Proceso"
boolean checked = true
end type

event clicked;ii_informe	=	1

cbx_todospallet.Enabled 	= 	FALSE
cbx_todospallet.Checked 	= 	TRUE
cbx_conscajas.Enabled		=	False
cbx_conscajas.Checked		= 	False

cbx_todospallet.TriggerEvent("clicked")
end event

type rb_2 from radiobutton within w_info_kilos_por_proceso
boolean visible = false
integer x = 3730
integer y = 276
integer width = 855
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Pallet / Pucho Por Proceso"
end type

event clicked;ii_informe	=	2

cbx_todospallet.Enabled 	= 	True
cbx_todospallet.Checked 	= 	True
cbx_conscajas.Enabled		=	True
cbx_conscajas.Checked		= 	True

cbx_todospallet.TriggerEvent("clicked")
end event

type cbx_conscajas from checkbox within w_info_kilos_por_proceso
boolean visible = false
integer x = 3790
integer y = 1544
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Consolida Cajas"
end type

type dw_1 from datawindow within w_info_kilos_por_proceso
boolean visible = false
integer x = 3323
integer y = 80
integer width = 210
integer height = 148
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_kilos_por_proceso"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

