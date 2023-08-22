$PBExportHeader$w_info_produccion_resumido.srw
forward
global type w_info_produccion_resumido from w_para_informes
end type
type gb_3 from groupbox within w_info_produccion_resumido
end type
type st_6 from statictext within w_info_produccion_resumido
end type
type st_1 from statictext within w_info_produccion_resumido
end type
type st_8 from statictext within w_info_produccion_resumido
end type
type st_3 from statictext within w_info_produccion_resumido
end type
type st_variedad from statictext within w_info_produccion_resumido
end type
type st_11 from statictext within w_info_produccion_resumido
end type
type st_embalaje from statictext within w_info_produccion_resumido
end type
type st_calidad from statictext within w_info_produccion_resumido
end type
type st_5 from statictext within w_info_produccion_resumido
end type
type st_4 from statictext within w_info_produccion_resumido
end type
type uo_selplanta from uo_seleccion_plantas within w_info_produccion_resumido
end type
type uo_selproductor from uo_seleccion_productor within w_info_produccion_resumido
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_produccion_resumido
end type
type cbx_etiqueta from checkbox within w_info_produccion_resumido
end type
type cbx_consetiqueta from checkbox within w_info_produccion_resumido
end type
type dw_etiqueta from datawindow within w_info_produccion_resumido
end type
type cbx_embalaje from checkbox within w_info_produccion_resumido
end type
type em_embalaje from editmask within w_info_produccion_resumido
end type
type cbx_consembalaje from checkbox within w_info_produccion_resumido
end type
type cbx_conscalidad from checkbox within w_info_produccion_resumido
end type
type cbx_calidad from checkbox within w_info_produccion_resumido
end type
type em_calidad from editmask within w_info_produccion_resumido
end type
type cb_buscaembalaje from commandbutton within w_info_produccion_resumido
end type
type st_2 from statictext within w_info_produccion_resumido
end type
type em_desde from editmask within w_info_produccion_resumido
end type
type st_7 from statictext within w_info_produccion_resumido
end type
type em_hasta from editmask within w_info_produccion_resumido
end type
type cbx_fecemb from checkbox within w_info_produccion_resumido
end type
type em_orden from editmask within w_info_produccion_resumido
end type
type cbx_ordentodo from checkbox within w_info_produccion_resumido
end type
type st_17 from statictext within w_info_produccion_resumido
end type
type cbx_todosfecha from checkbox within w_info_produccion_resumido
end type
type r_1 from rectangle within w_info_produccion_resumido
end type
type gb_4 from groupbox within w_info_produccion_resumido
end type
type st_10 from statictext within w_info_produccion_resumido
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_produccion_resumido
end type
type cbx_consproc from checkbox within w_info_produccion_resumido
end type
type cbx_exporta from checkbox within w_info_produccion_resumido
end type
type sle_estado from singlelineedit within w_info_produccion_resumido
end type
type st_9 from statictext within w_info_produccion_resumido
end type
type uo_selcategoria from uo_seleccion_categoria within w_info_produccion_resumido
end type
type st_12 from statictext within w_info_produccion_resumido
end type
type uo_selespecie from uo_seleccion_especie within w_info_produccion_resumido
end type
end forward

global type w_info_produccion_resumido from w_para_informes
integer width = 3986
integer height = 2084
string title = "Informe de Cajas Embaladas"
boolean minbox = false
boolean maxbox = false
gb_3 gb_3
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
st_10 st_10
uo_selcliente uo_selcliente
cbx_consproc cbx_consproc
cbx_exporta cbx_exporta
sle_estado sle_estado
st_9 st_9
uo_selcategoria uo_selcategoria
st_12 st_12
uo_selespecie uo_selespecie
end type
global w_info_produccion_resumido w_info_produccion_resumido

type variables
str_mant				istr_mant

Integer				ii_nroorden, ii_tipoorden, ii_cliente

DataWindowChild	idwc_etiqueta
end variables

forward prototypes
public function boolean noexisteetiqueta (integer li_etiqueta)
end prototypes

public function boolean noexisteetiqueta (integer li_etiqueta);String	ls_nombre


SELECT	etiq_nombre
INTO    :ls_nombre
FROM	dba.etiquetas
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

on w_info_produccion_resumido.create
int iCurrent
call super::create
this.gb_3=create gb_3
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
this.st_10=create st_10
this.uo_selcliente=create uo_selcliente
this.cbx_consproc=create cbx_consproc
this.cbx_exporta=create cbx_exporta
this.sle_estado=create sle_estado
this.st_9=create st_9
this.uo_selcategoria=create uo_selcategoria
this.st_12=create st_12
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_11
this.Control[iCurrent+8]=this.st_embalaje
this.Control[iCurrent+9]=this.st_calidad
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.st_4
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
this.Control[iCurrent+36]=this.st_10
this.Control[iCurrent+37]=this.uo_selcliente
this.Control[iCurrent+38]=this.cbx_consproc
this.Control[iCurrent+39]=this.cbx_exporta
this.Control[iCurrent+40]=this.sle_estado
this.Control[iCurrent+41]=this.st_9
this.Control[iCurrent+42]=this.uo_selcategoria
this.Control[iCurrent+43]=this.st_12
this.Control[iCurrent+44]=this.uo_selespecie
end on

on w_info_produccion_resumido.destroy
call super::destroy
destroy(this.gb_3)
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
destroy(this.st_10)
destroy(this.uo_selcliente)
destroy(this.cbx_consproc)
destroy(this.cbx_exporta)
destroy(this.sle_estado)
destroy(this.st_9)
destroy(this.uo_selcategoria)
destroy(this.st_12)
destroy(this.uo_selespecie)
end on

event open;call super::open;Boolean lb_cerrar

IF IsNull(uo_SelCliente.Codigo) 		THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) 		THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo)	THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.Codigo) 		THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelCategoria.Codigo) 	THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
END IF

uo_SelProductor.Filtra(-1)

dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()
dw_etiqueta.InsertRow(0)

cbx_todosfecha.TriggerEvent("clicked")


em_desde.Text 		= 	String(RelativeDate(Today(), -365))
em_hasta.Text 		= 	String(Today())

ii_TipoOrden		=	Integer(Message.StringParm)
end event

type pb_excel from w_para_informes`pb_excel within w_info_produccion_resumido
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_computador from w_para_informes`st_computador within w_info_produccion_resumido
integer x = 3031
end type

type st_usuario from w_para_informes`st_usuario within w_info_produccion_resumido
integer x = 3031
end type

type st_temporada from w_para_informes`st_temporada within w_info_produccion_resumido
integer x = 3031
end type

type p_logo from w_para_informes`p_logo within w_info_produccion_resumido
end type

type st_titulo from w_para_informes`st_titulo within w_info_produccion_resumido
integer width = 3259
string text = "Informe de Cajas Embaladas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_produccion_resumido
integer x = 3621
integer y = 1312
integer taborder = 100
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

event pb_acepta::clicked;Integer		li_etiqueta, 	li_consfecha
String		ls_embalaje, 	ls_calidad, 	ls_Archivo,	ls_ruta
Long			ll_nroorden, 	fila
Date			ld_desde, 		ld_hasta

SetPointer(HourGlass!)

IF cbx_exporta.Checked THEN
	sle_estado.text	=	"Iniciando Exportación"
END IF

istr_info.titulo	= "INFORME DE CAJAS EMBALADAS"
istr_info.copias	= 1

IF cbx_ordentodo.Checked THEN
	ll_nroOrden	= 	-1
	IF cbx_consproc.Checked THEN	ll_nroOrden	= 	-9
ELSE
	ll_nroOrden 	= 	Long(em_orden.Text)
END IF

IF cbx_embalaje.Checked THEN
	ls_embalaje 	= 'Z'
	IF cbx_consembalaje.Checked THEN	ls_embalaje 	= '-9'
ELSE
	ls_embalaje 	= 	em_embalaje.Text
END IF

IF cbx_calidad.Checked THEN
	ls_calidad 	= 'Z'
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

IF NOT cbx_exporta.Checked THEN
	vinf.dw_1.DataObject = "dw_info_emba_cajas_encab"
ELSE
	vinf.dw_1.DataObject = "dw_info_emba_cajas_excel"
END IF

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_SelPlanta.Codigo, ii_tipoorden,ll_nroorden,&
								 uo_SelProductor.Codigo,uo_SelEspecie.Codigo,uo_SelVariedad.Codigo, ls_embalaje,&
								 ls_calidad,li_etiqueta,ld_desde, ld_hasta,li_ConsFecha, uo_SelCategoria.codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("Cliente.text = '" + uo_SelCliente.Nombre + "'")
	
	IF cbx_exporta.Checked THEN

		RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
		
		ls_Archivo				= '\Cajas Embaladas del ' + String(ld_desde, 'dd.mm.yyyy') + " al " + String(ld_desde, 'dd.mm.yyyy') + '.xls'
		vinf.dw_1.SaveAs(ls_ruta + ls_archivo, Excel!,True) 
		sle_estado.text		= 	"Listo"
		Close(vinf)
	ELSE
		IF gs_Ambiente <> 'Windows' THEN
				F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		
		END IF
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_produccion_resumido
integer x = 3621
integer y = 1668
integer taborder = 150
fontcharset fontcharset = ansi!
end type

type gb_3 from groupbox within w_info_produccion_resumido
integer x = 379
integer y = 1656
integer width = 3031
integer height = 196
integer taborder = 170
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
end type

type st_6 from statictext within w_info_produccion_resumido
integer x = 297
integer y = 568
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

type st_1 from statictext within w_info_produccion_resumido
integer x = 297
integer y = 740
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

type st_8 from statictext within w_info_produccion_resumido
integer x = 297
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

type st_3 from statictext within w_info_produccion_resumido
integer x = 297
integer y = 1120
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

type st_variedad from statictext within w_info_produccion_resumido
integer x = 297
integer y = 1272
integer width = 302
integer height = 96
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

type st_11 from statictext within w_info_produccion_resumido
integer x = 1865
integer y = 740
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

type st_embalaje from statictext within w_info_produccion_resumido
integer x = 1865
integer y = 924
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

type st_calidad from statictext within w_info_produccion_resumido
integer x = 1870
integer y = 1092
integer width = 256
integer height = 92
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
string text = "Calibre"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_produccion_resumido
integer x = 1787
integer y = 424
integer width = 1714
integer height = 980
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

type st_4 from statictext within w_info_produccion_resumido
integer x = 242
integer y = 424
integer width = 1545
integer height = 980
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

type uo_selplanta from uo_seleccion_plantas within w_info_produccion_resumido
event destroy ( )
integer x = 805
integer y = 640
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_produccion_resumido
event destroy ( )
integer x = 805
integer y = 824
integer taborder = 50
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_produccion_resumido
event destroy ( )
integer x = 805
integer y = 1192
integer taborder = 90
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_etiqueta from checkbox within w_info_produccion_resumido
integer x = 2226
integer y = 640
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

type cbx_consetiqueta from checkbox within w_info_produccion_resumido
integer x = 2711
integer y = 640
integer width = 471
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
string text = "Consolidadas"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[17]	=	'-9'
ELSE
	istr_mant.argumento[17]	=	'0'
END IF

end event

type dw_etiqueta from datawindow within w_info_produccion_resumido
integer x = 2213
integer y = 708
integer width = 905
integer height = 96
integer taborder = 40
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

type cbx_embalaje from checkbox within w_info_produccion_resumido
integer x = 2226
integer y = 836
integer width = 402
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

type em_embalaje from editmask within w_info_produccion_resumido
integer x = 2231
integer y = 908
integer width = 297
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxx"
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
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

type cbx_consembalaje from checkbox within w_info_produccion_resumido
integer x = 2711
integer y = 836
integer width = 471
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
string text = "Consolidados"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'.9'
ELSE
	istr_mant.argumento[16]	=	'0'
END IF

end event

type cbx_conscalidad from checkbox within w_info_produccion_resumido
boolean visible = false
integer x = 2496
integer y = 788
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
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

type cbx_calidad from checkbox within w_info_produccion_resumido
integer x = 2226
integer y = 1020
integer width = 297
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

type em_calidad from editmask within w_info_produccion_resumido
integer x = 2231
integer y = 1092
integer width = 297
integer height = 92
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
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

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
li_especie	=	Integer(istr_mant.argumento[2]) // Especie
li_variedad	=	Integer(istr_mant.argumento[5]) // Variedad
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

type cb_buscaembalaje from commandbutton within w_info_produccion_resumido
boolean visible = false
integer x = 2318
integer y = 660
integer width = 119
integer height = 100
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "..."
end type

type st_2 from statictext within w_info_produccion_resumido
integer x = 448
integer y = 1740
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

type em_desde from editmask within w_info_produccion_resumido
integer x = 814
integer y = 1724
integer width = 462
integer height = 96
integer taborder = 130
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

type st_7 from statictext within w_info_produccion_resumido
integer x = 1326
integer y = 1740
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

type em_hasta from editmask within w_info_produccion_resumido
integer x = 1605
integer y = 1724
integer width = 462
integer height = 96
integer taborder = 140
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

type cbx_fecemb from checkbox within w_info_produccion_resumido
integer x = 2711
integer y = 1732
integer width = 443
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

type em_orden from editmask within w_info_produccion_resumido
integer x = 955
integer y = 1508
integer width = 375
integer height = 96
integer taborder = 120
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

type cbx_ordentodo from checkbox within w_info_produccion_resumido
integer x = 2226
integer y = 1520
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

type st_17 from statictext within w_info_produccion_resumido
integer x = 448
integer y = 1524
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

type cbx_todosfecha from checkbox within w_info_produccion_resumido
integer x = 2226
integer y = 1736
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

type r_1 from rectangle within w_info_produccion_resumido
integer linethickness = 4
long fillcolor = 16777215
integer x = 2587
integer y = 1560
integer width = 229
integer height = 200
end type

type gb_4 from groupbox within w_info_produccion_resumido
integer x = 379
integer y = 1440
integer width = 3031
integer height = 196
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
end type

type st_10 from statictext within w_info_produccion_resumido
integer x = 242
integer y = 1408
integer width = 3259
integer height = 516
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_produccion_resumido
integer x = 805
integer y = 456
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type cbx_consproc from checkbox within w_info_produccion_resumido
integer x = 2711
integer y = 1516
integer width = 443
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

type cbx_exporta from checkbox within w_info_produccion_resumido
integer x = 2953
integer y = 1284
integer width = 507
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
string text = "Exportar a Excel"
end type

event clicked;IF This.Checked THEN
	sle_estado.Text		=	"En Espera"
ELSE
	sle_estado.Text		=	""
END IF
end event

type sle_estado from singlelineedit within w_info_produccion_resumido
integer x = 2231
integer y = 1272
integer width = 699
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 10789024
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_9 from statictext within w_info_produccion_resumido
integer x = 1865
integer y = 556
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
string text = "Categoria"
boolean focusrectangle = false
end type

type uo_selcategoria from uo_seleccion_categoria within w_info_produccion_resumido
integer x = 2226
integer y = 456
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcategoria.destroy
call uo_seleccion_categoria::destroy
end on

type st_12 from statictext within w_info_produccion_resumido
integer x = 1870
integer y = 1272
integer width = 256
integer height = 92
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
string text = "Excel"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_produccion_resumido
integer x = 805
integer y = 1008
integer taborder = 60
boolean bringtotop = true
end type

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

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

