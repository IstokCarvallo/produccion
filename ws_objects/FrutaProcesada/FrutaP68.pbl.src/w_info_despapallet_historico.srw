$PBExportHeader$w_info_despapallet_historico.srw
$PBExportComments$informe de despaho de pallet historico
forward
global type w_info_despapallet_historico from w_para_informes
end type
type st_4 from statictext within w_info_despapallet_historico
end type
type st_1 from statictext within w_info_despapallet_historico
end type
type st_2 from statictext within w_info_despapallet_historico
end type
type em_desde from editmask within w_info_despapallet_historico
end type
type dw_cliente from datawindow within w_info_despapallet_historico
end type
type st_6 from statictext within w_info_despapallet_historico
end type
type st_3 from statictext within w_info_despapallet_historico
end type
type dw_especie from datawindow within w_info_despapallet_historico
end type
type cbx_planta from checkbox within w_info_despapallet_historico
end type
type st_7 from statictext within w_info_despapallet_historico
end type
type em_hasta from editmask within w_info_despapallet_historico
end type
type st_8 from statictext within w_info_despapallet_historico
end type
type cbx_productor from checkbox within w_info_despapallet_historico
end type
type st_variedad from statictext within w_info_despapallet_historico
end type
type cbx_variedad from checkbox within w_info_despapallet_historico
end type
type cb_buscavariedad from commandbutton within w_info_despapallet_historico
end type
type sle_variedad from singlelineedit within w_info_despapallet_historico
end type
type st_embalaje from statictext within w_info_despapallet_historico
end type
type cbx_embalaje from checkbox within w_info_despapallet_historico
end type
type cb_buscaembalaje from commandbutton within w_info_despapallet_historico
end type
type cbx_calidad from checkbox within w_info_despapallet_historico
end type
type em_calidad from editmask within w_info_despapallet_historico
end type
type st_12 from statictext within w_info_despapallet_historico
end type
type gb_3 from groupbox within w_info_despapallet_historico
end type
type em_embalaje from editmask within w_info_despapallet_historico
end type
type em_variedad from editmask within w_info_despapallet_historico
end type
type dw_productor from datawindow within w_info_despapallet_historico
end type
type dw_planta from datawindow within w_info_despapallet_historico
end type
type st_5 from statictext within w_info_despapallet_historico
end type
type em_numdes from editmask within w_info_despapallet_historico
end type
type st_9 from statictext within w_info_despapallet_historico
end type
type em_numhas from editmask within w_info_despapallet_historico
end type
type gb_4 from groupbox within w_info_despapallet_historico
end type
type st_10 from statictext within w_info_despapallet_historico
end type
type dw_destino from datawindow within w_info_despapallet_historico
end type
type st_11 from statictext within w_info_despapallet_historico
end type
type st_13 from statictext within w_info_despapallet_historico
end type
type st_15 from statictext within w_info_despapallet_historico
end type
type dw_tipoprodu from datawindow within w_info_despapallet_historico
end type
type cbx_tipoproductor from checkbox within w_info_despapallet_historico
end type
type cbx_destino from checkbox within w_info_despapallet_historico
end type
end forward

global type w_info_despapallet_historico from w_para_informes
integer x = 14
integer y = 32
integer width = 2903
integer height = 2368
string title = "Pallets Históricos Embarcados"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
st_3 st_3
dw_especie dw_especie
cbx_planta cbx_planta
st_7 st_7
em_hasta em_hasta
st_8 st_8
cbx_productor cbx_productor
st_variedad st_variedad
cbx_variedad cbx_variedad
cb_buscavariedad cb_buscavariedad
sle_variedad sle_variedad
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_calidad cbx_calidad
em_calidad em_calidad
st_12 st_12
gb_3 gb_3
em_embalaje em_embalaje
em_variedad em_variedad
dw_productor dw_productor
dw_planta dw_planta
st_5 st_5
em_numdes em_numdes
st_9 st_9
em_numhas em_numhas
gb_4 gb_4
st_10 st_10
dw_destino dw_destino
st_11 st_11
st_13 st_13
st_15 st_15
dw_tipoprodu dw_tipoprodu
cbx_tipoproductor cbx_tipoproductor
cbx_destino cbx_destino
end type
global w_info_despapallet_historico w_info_despapallet_historico

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie, idwc_productor,&
						idwc_destino

String 	is_NomPlanta, is_Embarque, is_Operacion, is_NomEmbarque
Integer	ii_Operacion

uo_cliente			iuo_cliente
uo_plantadesp		iuo_plantadesp
uo_destinos			iuo_destinos
uo_tipoproductor	iuo_tipoproductor
uo_especie			iuo_especie
uo_calibre								iuo_calibre
end variables

on w_info_despapallet_historico.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_3=create st_3
this.dw_especie=create dw_especie
this.cbx_planta=create cbx_planta
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.cbx_productor=create cbx_productor
this.st_variedad=create st_variedad
this.cbx_variedad=create cbx_variedad
this.cb_buscavariedad=create cb_buscavariedad
this.sle_variedad=create sle_variedad
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_calidad=create cbx_calidad
this.em_calidad=create em_calidad
this.st_12=create st_12
this.gb_3=create gb_3
this.em_embalaje=create em_embalaje
this.em_variedad=create em_variedad
this.dw_productor=create dw_productor
this.dw_planta=create dw_planta
this.st_5=create st_5
this.em_numdes=create em_numdes
this.st_9=create st_9
this.em_numhas=create em_numhas
this.gb_4=create gb_4
this.st_10=create st_10
this.dw_destino=create dw_destino
this.st_11=create st_11
this.st_13=create st_13
this.st_15=create st_15
this.dw_tipoprodu=create dw_tipoprodu
this.cbx_tipoproductor=create cbx_tipoproductor
this.cbx_destino=create cbx_destino
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.dw_especie
this.Control[iCurrent+9]=this.cbx_planta
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.em_hasta
this.Control[iCurrent+12]=this.st_8
this.Control[iCurrent+13]=this.cbx_productor
this.Control[iCurrent+14]=this.st_variedad
this.Control[iCurrent+15]=this.cbx_variedad
this.Control[iCurrent+16]=this.cb_buscavariedad
this.Control[iCurrent+17]=this.sle_variedad
this.Control[iCurrent+18]=this.st_embalaje
this.Control[iCurrent+19]=this.cbx_embalaje
this.Control[iCurrent+20]=this.cb_buscaembalaje
this.Control[iCurrent+21]=this.cbx_calidad
this.Control[iCurrent+22]=this.em_calidad
this.Control[iCurrent+23]=this.st_12
this.Control[iCurrent+24]=this.gb_3
this.Control[iCurrent+25]=this.em_embalaje
this.Control[iCurrent+26]=this.em_variedad
this.Control[iCurrent+27]=this.dw_productor
this.Control[iCurrent+28]=this.dw_planta
this.Control[iCurrent+29]=this.st_5
this.Control[iCurrent+30]=this.em_numdes
this.Control[iCurrent+31]=this.st_9
this.Control[iCurrent+32]=this.em_numhas
this.Control[iCurrent+33]=this.gb_4
this.Control[iCurrent+34]=this.st_10
this.Control[iCurrent+35]=this.dw_destino
this.Control[iCurrent+36]=this.st_11
this.Control[iCurrent+37]=this.st_13
this.Control[iCurrent+38]=this.st_15
this.Control[iCurrent+39]=this.dw_tipoprodu
this.Control[iCurrent+40]=this.cbx_tipoproductor
this.Control[iCurrent+41]=this.cbx_destino
end on

on w_info_despapallet_historico.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.dw_especie)
destroy(this.cbx_planta)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.cbx_productor)
destroy(this.st_variedad)
destroy(this.cbx_variedad)
destroy(this.cb_buscavariedad)
destroy(this.sle_variedad)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_calidad)
destroy(this.em_calidad)
destroy(this.st_12)
destroy(this.gb_3)
destroy(this.em_embalaje)
destroy(this.em_variedad)
destroy(this.dw_productor)
destroy(this.dw_planta)
destroy(this.st_5)
destroy(this.em_numdes)
destroy(this.st_9)
destroy(this.em_numhas)
destroy(this.gb_4)
destroy(this.st_10)
destroy(this.dw_destino)
destroy(this.st_11)
destroy(this.st_13)
destroy(this.st_15)
destroy(this.dw_tipoprodu)
destroy(this.cbx_tipoproductor)
destroy(this.cbx_destino)
end on

event open;call super::open;x	=	0
y	=	0

iuo_cliente			=	CREATE	uo_cliente			
iuo_plantadesp		=	CREATE	uo_plantadesp		
iuo_destinos		=	CREATE	uo_destinos			
iuo_tipoproductor	=	CREATE	uo_tipoproductor	
iuo_especie			=	CREATE	uo_especie	
iuo_calibre   						=	Create uo_calibre

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve(gi_CodExport)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

dw_tipoprodu.GetChild("tipr_codigo", idwc_tipopro)
idwc_tipopro.SetTransObject(sqlca)
idwc_tipopro.Retrieve()
dw_tipoprodu.InsertRow(0)

//dw_productor.GetChild("prod_codigo", idwc_productor)
//idwc_productor.SetTransObject(sqlca)
//idwc_productor.Retrieve()
//dw_productor.InsertRow(0)

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)

dw_destino.GetChild("dest_codigo", idwc_destino)
idwc_destino.SetTransObject(sqlca)
idwc_destino.Retrieve(0)
dw_destino.InsertRow(0)


em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())

end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_despapallet_historico
end type

type st_usuario from w_para_informes`st_usuario within w_info_despapallet_historico
end type

type st_temporada from w_para_informes`st_temporada within w_info_despapallet_historico
end type

type p_logo from w_para_informes`p_logo within w_info_despapallet_historico
end type

type st_titulo from w_para_informes`st_titulo within w_info_despapallet_historico
integer width = 2075
string text = "Despacho Historico de pallet"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_despapallet_historico
string tag = "Imprimir Reporte"
integer x = 2450
integer y = 1688
integer taborder = 230
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_especie, li_destino, li_tipoprod, li_variedad
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, texto_numero, &
         ls_embalaje, ls_calibre, ls_planta, ls_destino, ls_cliente,ls_tipoprod
Long		ll_numdes, ll_numhas			

istr_info.titulo	= 'LISTADO DE PALLETS HISTORICOS DESPACHADOS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_despachopallethistorico"

li_cliente 		=	dw_cliente.Object.clie_codigo[1]

  SELECT clie_nombre  
    INTO :ls_cliente  
    FROM dba.clientesprod  
   WHERE clie_codigo = :li_cliente   ;

IF cbx_planta.Checked THEN
	li_planta	=	-1
	ls_planta   =	'TODAS'
ELSE
	li_planta	=	dw_planta.Object.plde_codigo[1]
	IF Isnull(li_planta) OR li_planta = 0 THEN
		Messagebox("Atención","Debe Ingresar una Planta",exclamation!)
		RETURN 
	ELSE
		ls_planta = iuo_plantadesp.nombre
	END IF
END IF

IF cbx_destino.Checked THEN
	li_destino	=	-1
	ls_destino  = 'TODOS'
ELSE
	li_destino	=	dw_destino.Object.dest_codigo[1]
	IF Isnull(li_destino) OR li_destino = 0 THEN
		Messagebox("Atención","Debe Ingresar un Destino",exclamation!)
		RETURN 
	ELSE
		ls_destino = iuo_destinos.nombre
	END IF
END IF

IF cbx_tipoproductor.Checked THEN
	li_tipoprod	=	-1
	ls_tipoprod = 'TODOS'
ELSE
	li_tipoprod	=	dw_tipoprodu.Object.tipr_codigo[1]
	IF Isnull(li_tipoprod) OR li_tipoprod = 0 THEN
		Messagebox("Atención","Debe Ingresar un Destino",exclamation!)
		RETURN 
	ELSE
		ls_tipoprod = iuo_tipoproductor.nombre
	END IF
END IF

li_especie	=	dw_especie.Object.espe_codigo[1]


ld_desde			=	Date(em_desde.text)
ld_hasta			=	Date(em_hasta.text)
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

ll_numdes		=	Long(em_numdes.text)
IF Isnull(ll_numdes) or ll_numdes = 0 THEN
	Messagebox("Atención","Debe Ingresar Número de Inspección Desde",exclamation!)
	em_numdes.SetFocus()
	RETURN 
END IF
	
ll_numhas		=	Long(em_numhas.text)
IF Isnull(ll_numhas) or ll_numhas = 0 THEN
	Messagebox("Atención","Debe Ingresar Número de Inspección Hasta",exclamation!)
	em_numhas.SetFocus()
	RETURN 
END IF
texto_numero	=	"Desde El :  " + em_numdes.text + "   Hasta El :  " + em_numhas.text

IF cbx_variedad.Checked THEN
	li_variedad = -1
ELSE
	li_variedad = Integer(em_variedad.Text)
END IF

IF cbx_calidad.Checked THEN
	ls_Calibre = '*'
ELSE
	ls_Calibre = em_calidad.Text
END IF

IF cbx_embalaje.Checked THEN
	ls_embalaje = '*'
ELSE
	ls_embalaje = em_embalaje.Text
END IF


vinf.dw_1.SetTransObject(sqlca)



fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, li_tipoprod,li_especie,li_variedad,ls_calibre,ls_embalaje, ld_desde, ld_hasta, li_destino, ll_numdes, ll_numhas)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("cliente.text = '" + ls_Cliente + "'")
		vinf.dw_1.Modify("Planta.text = '" + ls_planta + "'")
		vinf.dw_1.Modify("destino.text = '" + ls_destino + "'")
		vinf.dw_1.Modify("tipoprod.text = '" + ls_tipoprod + "'")
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		vinf.dw_1.Modify("numero.text = '" + texto_numero + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_despapallet_historico
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2450
integer y = 1976
integer taborder = 240
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_despapallet_historico
integer x = 256
integer y = 440
integer width = 2075
integer height = 712
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_despapallet_historico
integer x = 334
integer y = 636
integer width = 229
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_despapallet_historico
integer x = 334
integer y = 1744
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_despapallet_historico
integer x = 786
integer y = 1712
integer width = 393
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
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type dw_cliente from datawindow within w_info_despapallet_historico
integer x = 791
integer y = 476
integer width = 1147
integer height = 104
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF NOT iuo_Cliente.Existe(Integer(data),TRUE,sqlca) THEN
	This.SetItem(1, "clie_codigo", li_Null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_despapallet_historico
integer x = 334
integer y = 504
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_despapallet_historico
integer x = 334
integer y = 1028
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_despapallet_historico
integer x = 791
integer y = 1012
integer width = 882
integer height = 100
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF NOT iuo_especie.Existe(Integer(data),TRUE,sqlca) THEN
	This.SetItem(1, "espe_codigo", li_Null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_planta from checkbox within w_info_despapallet_historico
integer x = 2007
integer y = 640
integer width = 297
integer height = 72
integer taborder = 20
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

event clicked;IF This.Checked THEN
	dw_planta.Enabled			=	False
	istr_mant.argumento[2]	=	'0'
ELSE
	dw_planta.Enabled			=	True
	dw_planta.SetFocus()
END IF
end event

type st_7 from statictext within w_info_despapallet_historico
integer x = 1266
integer y = 1756
integer width = 366
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_despapallet_historico
integer x = 1577
integer y = 1712
integer width = 393
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_8 from statictext within w_info_despapallet_historico
integer x = 334
integer y = 896
integer width = 443
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Tipo Productor"
boolean focusrectangle = false
end type

type cbx_productor from checkbox within w_info_despapallet_historico
boolean visible = false
integer x = 2551
integer y = 540
integer width = 297
integer height = 72
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;//IF cbx_productor.Checked = TRUE THEN
//	dw_productor.Enabled  = False
//	dw_packing.Enabled    = False
//	cbx_packing.Checked   = False
//	dw_productor.Reset()
//	dw_productor.insertrow(0)
//	istr_mant.argumento[6]	=	'0'
//ELSE
//	dw_productor.Enabled  = True
//	dw_productor.SetFocus()
//	dw_productor.Reset()
//	dw_productor.InsertRow(0)
//	dw_packing.Enabled    = True
//	cbx_packing.Enabled   = True
//	dw_packing.Reset()
//	dw_packing.InsertRow(0)	
//END IF
//	
end event

type st_variedad from statictext within w_info_despapallet_historico
integer x = 334
integer y = 1216
integer width = 279
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type cbx_variedad from checkbox within w_info_despapallet_historico
integer x = 2007
integer y = 1212
integer width = 297
integer height = 72
integer taborder = 70
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

event clicked;IF This.Checked THEN
	em_variedad.Enabled			=	False
	cb_buscavariedad.Enabled	=	False
	em_variedad.Text				=	''
	sle_variedad.Text				=	''
	istr_mant.argumento[9]		=	'0'
ELSE
	em_variedad.Enabled			=	True
	cb_buscavariedad.Enabled	=	True
END IF
end event

type cb_buscavariedad from commandbutton within w_info_despapallet_historico
integer x = 1065
integer y = 1208
integer width = 96
integer height = 84
integer taborder = 90
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

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente
lstr_busq.argum[2]	=	istr_mant.argumento[5] // Especie

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[4] = "" THEN
	em_variedad.SetFocus()
ELSE
	em_variedad.Text			=	lstr_busq.argum[4]
	sle_variedad.Text			=	lstr_busq.argum[5]
	istr_mant.argumento[9]	=	lstr_busq.argum[4]
END IF

end event

type sle_variedad from singlelineedit within w_info_despapallet_historico
integer x = 1166
integer y = 1200
integer width = 805
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type st_embalaje from statictext within w_info_despapallet_historico
integer x = 334
integer y = 1500
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
long backcolor = 553648127
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_despapallet_historico
integer x = 1184
integer y = 1492
integer width = 297
integer height = 72
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
ELSE
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_despapallet_historico
integer x = 1065
integer y = 1488
integer width = 96
integer height = 84
integer taborder = 210
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

event clicked;
Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
END IF
end event

type cbx_calidad from checkbox within w_info_despapallet_historico
integer x = 1184
integer y = 1344
integer width = 297
integer height = 72
integer taborder = 100
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

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[28]	=	'-1'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type em_calidad from editmask within w_info_despapallet_historico
integer x = 795
integer y = 1336
integer width = 261
integer height = 96
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

event modified;Integer	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	
	li_especie	=	dw_especie.Object.espe_codigo[1] // Especie
	li_variedad	=	Integer(em_variedad.text) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	

end event

type st_12 from statictext within w_info_despapallet_historico
integer x = 334
integer y = 1352
integer width = 256
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Calidad"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_despapallet_historico
integer x = 283
integer y = 1624
integer width = 1911
integer height = 232
integer taborder = 250
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fecha Inspección"
end type

type em_embalaje from editmask within w_info_despapallet_historico
integer x = 795
integer y = 1480
integer width = 261
integer height = 96
integer taborder = 200
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
string mask = "xxxxxxxxxx"
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	dw_cliente.Object.clie_codigo[1] // Cliente
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
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
END IF

end event

type em_variedad from editmask within w_info_despapallet_historico
integer x = 791
integer y = 1200
integer width = 261
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "####"
end type

event modified;Integer		li_especie, li_variedad
String		ls_Nombre

li_especie	=	Integer(dw_especie.Object.espe_codigo[1]) // Especie
li_variedad	=	Integer(This.Text)

SELECT	vari_nombre
	INTO	:ls_Nombre
	FROM	dba.variedades
	WHERE	espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variedades")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Variedad no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	sle_variedad.Text			=	ls_nombre
END IF
end event

type dw_productor from datawindow within w_info_despapallet_historico
boolean visible = false
integer x = 2647
integer y = 896
integer width = 983
integer height = 92
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;RETURN 1
end event

event itemchanged;//String	 ls_null
//SetNull(ls_null)
//
//	IF ExisteProductor(Long(data)) THEN
//	istr_mant.argumento[6]	=	data	
//	RETURN 0
//ELSE
//	This.SetItem(1, "prod_codigo", ls_null)
//	RETURN 1
//END IF
end event

type dw_planta from datawindow within w_info_despapallet_historico
integer x = 791
integer y = 612
integer width = 974
integer height = 92
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF NOT iuo_plantadesp.Existe(Integer(data),TRUE,sqlca) THEN
	This.SetItem(1, "plde_codigo", li_Null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_5 from statictext within w_info_despapallet_historico
integer x = 334
integer y = 2016
integer width = 206
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numdes from editmask within w_info_despapallet_historico
integer x = 786
integer y = 1996
integer width = 393
integer height = 96
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_9 from statictext within w_info_despapallet_historico
integer x = 1266
integer y = 2016
integer width = 297
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type em_numhas from editmask within w_info_despapallet_historico
integer x = 1577
integer y = 1996
integer width = 393
integer height = 96
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type gb_4 from groupbox within w_info_despapallet_historico
integer x = 283
integer y = 1912
integer width = 1911
integer height = 232
integer taborder = 260
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Nº de  Inspección"
end type

type st_10 from statictext within w_info_despapallet_historico
integer x = 256
integer y = 1900
integer width = 2075
integer height = 280
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_destino from datawindow within w_info_despapallet_historico
integer x = 791
integer y = 740
integer width = 887
integer height = 92
integer taborder = 270
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_destinos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF NOT iuo_destinos.Existe(Integer(data),TRUE,sqlca) THEN
	This.SetItem(1, "dest_codigo", li_Null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_11 from statictext within w_info_despapallet_historico
integer x = 334
integer y = 764
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Destino"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_despapallet_historico
integer x = 256
integer y = 1152
integer width = 2075
integer height = 464
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_despapallet_historico
integer x = 256
integer y = 1616
integer width = 2075
integer height = 280
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_tipoprodu from datawindow within w_info_despapallet_historico
integer x = 791
integer y = 876
integer width = 882
integer height = 92
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_tipoproduc"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF NOT iuo_tipoproductor.Existe(Integer(data),TRUE,sqlca) THEN
	This.SetItem(1, "tipr_codigo", li_Null)
	RETURN 1
END IF
end event

type cbx_tipoproductor from checkbox within w_info_despapallet_historico
integer x = 2007
integer y = 888
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_tipoprodu.Enabled			= 	False
	dw_tipoprodu.Object.tipr_codigo.BackGround.Color		=	RGB(166,180,210)
	istr_mant.argumento[3] =	"-1"

	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(sqlca)
	idwc_productor.Retrieve(integer(istr_mant.argumento[2]),integer(istr_mant.argumento[3]))
	dw_productor.InsertRow(0)

	
ELSE
	dw_tipoprodu.Enabled			= 	True
	dw_tipoprodu.Object.tipr_codigo.BackGround.Color		=	RGB(255, 255, 255)
	dw_tipoprodu.SetFocus()
END IF

end event

type cbx_destino from checkbox within w_info_despapallet_historico
integer x = 2007
integer y = 760
integer width = 297
integer height = 72
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_planta.Enabled			=	False
	istr_mant.argumento[2]	=	'0'
ELSE
	dw_planta.Enabled			=	True
	dw_planta.SetFocus()
END IF
end event

