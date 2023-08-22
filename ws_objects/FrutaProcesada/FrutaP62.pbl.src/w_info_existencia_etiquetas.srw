$PBExportHeader$w_info_existencia_etiquetas.srw
forward
global type w_info_existencia_etiquetas from w_para_informes
end type
type dw_cliente from datawindow within w_info_existencia_etiquetas
end type
type st_6 from statictext within w_info_existencia_etiquetas
end type
type st_4 from statictext within w_info_existencia_etiquetas
end type
type st_especie from statictext within w_info_existencia_etiquetas
end type
type st_nro2 from statictext within w_info_existencia_etiquetas
end type
type st_variedad from statictext within w_info_existencia_etiquetas
end type
type em_embalaje from editmask within w_info_existencia_etiquetas
end type
type st_embalaje from statictext within w_info_existencia_etiquetas
end type
type cbx_embalaje from checkbox within w_info_existencia_etiquetas
end type
type cb_buscaembalaje from commandbutton within w_info_existencia_etiquetas
end type
type st_5 from statictext within w_info_existencia_etiquetas
end type
type st_productor from statictext within w_info_existencia_etiquetas
end type
type em_productor from editmask within w_info_existencia_etiquetas
end type
type cb_buscaproductor from commandbutton within w_info_existencia_etiquetas
end type
type sle_productor from singlelineedit within w_info_existencia_etiquetas
end type
type cbx_productor from checkbox within w_info_existencia_etiquetas
end type
type cbx_planta from checkbox within w_info_existencia_etiquetas
end type
type dw_planta from datawindow within w_info_existencia_etiquetas
end type
type st_planta from statictext within w_info_existencia_etiquetas
end type
type st_55 from statictext within w_info_existencia_etiquetas
end type
type st_2 from statictext within w_info_existencia_etiquetas
end type
type em_desde from editmask within w_info_existencia_etiquetas
end type
type st_7 from statictext within w_info_existencia_etiquetas
end type
type em_hasta from editmask within w_info_existencia_etiquetas
end type
type rb_1 from radiobutton within w_info_existencia_etiquetas
end type
type rb_2 from radiobutton within w_info_existencia_etiquetas
end type
type cbx_consolprod from checkbox within w_info_existencia_etiquetas
end type
type cbx_consolplan from checkbox within w_info_existencia_etiquetas
end type
type rb_controltodos from radiobutton within w_info_existencia_etiquetas
end type
type rb_rechazados from radiobutton within w_info_existencia_etiquetas
end type
type rb_objetados from radiobutton within w_info_existencia_etiquetas
end type
type rb_habilitado from radiobutton within w_info_existencia_etiquetas
end type
type gb_6 from groupbox within w_info_existencia_etiquetas
end type
type st_1 from statictext within w_info_existencia_etiquetas
end type
type st_3 from statictext within w_info_existencia_etiquetas
end type
type cbx_status from checkbox within w_info_existencia_etiquetas
end type
type dw_stat from datawindow within w_info_existencia_etiquetas
end type
type uo_selespecie from uo_seleccion_especie within w_info_existencia_etiquetas
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_etiquetas
end type
type cbx_varirotula from checkbox within w_info_existencia_etiquetas
end type
type cbx_1 from checkbox within w_info_existencia_etiquetas
end type
type gb_3 from groupbox within w_info_existencia_etiquetas
end type
type st_14 from statictext within w_info_existencia_etiquetas
end type
type cbx_2 from checkbox within w_info_existencia_etiquetas
end type
end forward

global type w_info_existencia_etiquetas from w_para_informes
integer x = 14
integer y = 32
integer width = 3785
integer height = 2308
string title = "Existencia de Fruta"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_cliente dw_cliente
st_6 st_6
st_4 st_4
st_especie st_especie
st_nro2 st_nro2
st_variedad st_variedad
em_embalaje em_embalaje
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
cb_buscaembalaje cb_buscaembalaje
st_5 st_5
st_productor st_productor
em_productor em_productor
cb_buscaproductor cb_buscaproductor
sle_productor sle_productor
cbx_productor cbx_productor
cbx_planta cbx_planta
dw_planta dw_planta
st_planta st_planta
st_55 st_55
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
rb_1 rb_1
rb_2 rb_2
cbx_consolprod cbx_consolprod
cbx_consolplan cbx_consolplan
rb_controltodos rb_controltodos
rb_rechazados rb_rechazados
rb_objetados rb_objetados
rb_habilitado rb_habilitado
gb_6 gb_6
st_1 st_1
st_3 st_3
cbx_status cbx_status
dw_stat dw_stat
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
cbx_1 cbx_1
gb_3 gb_3
st_14 st_14
cbx_2 cbx_2
end type
global w_info_existencia_etiquetas w_info_existencia_etiquetas

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente, idwc_planta

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad

String is_NomPlanta,is_NomProductor, is_control_d
integer ii_control
end variables

forward prototypes
public function boolean noexistecliente (string cliente)
public function boolean noexisteplanta (string planta)
end prototypes

public function boolean noexistecliente (string cliente);Integer		li_cliente
String		ls_nombre

li_cliente	=	Integer(cliente)

SELECT	clie_nombre
	INTO	:ls_nombre
	FROM	dba.clientesprod
	WHERE	clie_codigo	=	:li_cliente;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla ClientesProd")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	dw_cliente.SetItem(1, "clie_codigo", li_cliente)
	istr_mant.argumento[1]	=	String(li_cliente)	
	RETURN False
END IF
end function

public function boolean noexisteplanta (string planta);Integer		li_planta

li_planta	=	Integer(planta)

SELECT	plde_nombre
	INTO	:is_NomPlanta
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:li_planta;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla PlantaDesp")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	dw_planta.SetItem(1, "plde_codigo", li_planta)
	istr_mant.argumento[6]	=	String(li_planta)	
	RETURN False
END IF
end function

on w_info_existencia_etiquetas.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_4=create st_4
this.st_especie=create st_especie
this.st_nro2=create st_nro2
this.st_variedad=create st_variedad
this.em_embalaje=create em_embalaje
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.st_5=create st_5
this.st_productor=create st_productor
this.em_productor=create em_productor
this.cb_buscaproductor=create cb_buscaproductor
this.sle_productor=create sle_productor
this.cbx_productor=create cbx_productor
this.cbx_planta=create cbx_planta
this.dw_planta=create dw_planta
this.st_planta=create st_planta
this.st_55=create st_55
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.rb_1=create rb_1
this.rb_2=create rb_2
this.cbx_consolprod=create cbx_consolprod
this.cbx_consolplan=create cbx_consolplan
this.rb_controltodos=create rb_controltodos
this.rb_rechazados=create rb_rechazados
this.rb_objetados=create rb_objetados
this.rb_habilitado=create rb_habilitado
this.gb_6=create gb_6
this.st_1=create st_1
this.st_3=create st_3
this.cbx_status=create cbx_status
this.dw_stat=create dw_stat
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.cbx_1=create cbx_1
this.gb_3=create gb_3
this.st_14=create st_14
this.cbx_2=create cbx_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_especie
this.Control[iCurrent+5]=this.st_nro2
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.em_embalaje
this.Control[iCurrent+8]=this.st_embalaje
this.Control[iCurrent+9]=this.cbx_embalaje
this.Control[iCurrent+10]=this.cb_buscaembalaje
this.Control[iCurrent+11]=this.st_5
this.Control[iCurrent+12]=this.st_productor
this.Control[iCurrent+13]=this.em_productor
this.Control[iCurrent+14]=this.cb_buscaproductor
this.Control[iCurrent+15]=this.sle_productor
this.Control[iCurrent+16]=this.cbx_productor
this.Control[iCurrent+17]=this.cbx_planta
this.Control[iCurrent+18]=this.dw_planta
this.Control[iCurrent+19]=this.st_planta
this.Control[iCurrent+20]=this.st_55
this.Control[iCurrent+21]=this.st_2
this.Control[iCurrent+22]=this.em_desde
this.Control[iCurrent+23]=this.st_7
this.Control[iCurrent+24]=this.em_hasta
this.Control[iCurrent+25]=this.rb_1
this.Control[iCurrent+26]=this.rb_2
this.Control[iCurrent+27]=this.cbx_consolprod
this.Control[iCurrent+28]=this.cbx_consolplan
this.Control[iCurrent+29]=this.rb_controltodos
this.Control[iCurrent+30]=this.rb_rechazados
this.Control[iCurrent+31]=this.rb_objetados
this.Control[iCurrent+32]=this.rb_habilitado
this.Control[iCurrent+33]=this.gb_6
this.Control[iCurrent+34]=this.st_1
this.Control[iCurrent+35]=this.st_3
this.Control[iCurrent+36]=this.cbx_status
this.Control[iCurrent+37]=this.dw_stat
this.Control[iCurrent+38]=this.uo_selespecie
this.Control[iCurrent+39]=this.uo_selvariedad
this.Control[iCurrent+40]=this.cbx_varirotula
this.Control[iCurrent+41]=this.cbx_1
this.Control[iCurrent+42]=this.gb_3
this.Control[iCurrent+43]=this.st_14
this.Control[iCurrent+44]=this.cbx_2
end on

on w_info_existencia_etiquetas.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_4)
destroy(this.st_especie)
destroy(this.st_nro2)
destroy(this.st_variedad)
destroy(this.em_embalaje)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.st_5)
destroy(this.st_productor)
destroy(this.em_productor)
destroy(this.cb_buscaproductor)
destroy(this.sle_productor)
destroy(this.cbx_productor)
destroy(this.cbx_planta)
destroy(this.dw_planta)
destroy(this.st_planta)
destroy(this.st_55)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.cbx_consolprod)
destroy(this.cbx_consolplan)
destroy(this.rb_controltodos)
destroy(this.rb_rechazados)
destroy(this.rb_objetados)
destroy(this.rb_habilitado)
destroy(this.gb_6)
destroy(this.st_1)
destroy(this.st_3)
destroy(this.cbx_status)
destroy(this.dw_stat)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.cbx_1)
destroy(this.gb_3)
destroy(this.st_14)
destroy(this.cbx_2)
end on

event open;Boolean	lb_Cerrar

x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_codexport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gi_codplanta)
//dw_planta.Modify("plde_nombre.BackGround.Color = " + String(RGB(166,180,210)))

// iuo_selespecie = Create uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

// iuo_selvariedad = Create uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
	uo_selvariedad.Enabled		=	False	
END IF

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[23]	= 	em_desde.Text
istr_mant.argumento[24]	=	em_hasta.Text
istr_mant.argumento[27]	= 	"0"//Consolidado Planta
istr_mant.argumento[28]	=	"0"//Consolidado Productor
istr_mant.argumento[1]	= 	String(gi_codexport)		//	Cliente
//istr_mant.argumento[2]	= 	String(gi_codespecie)	//	Especie
//istr_mant.argumento[3]	=	'0'							// Variedad
istr_mant.argumento[4]	=	'0'							// Productor
istr_mant.argumento[34]	=	'Todos'							// Productor
istr_mant.argumento[5]	=	'Z'							// Embalaje
istr_mant.argumento[6]	=	'0'							//	Planta
is_NomPlanta				=	"TODAS"
istr_mant.argumento[35]  =	'0'							// status
istr_mant.argumento[36]	=  "Todos"						// status nombre

istr_mant.argumento[9]	=	"1"

//*-------
ii_control 					= 	-1
is_control_d				=	"Todos"
end event

type st_computador from w_para_informes`st_computador within w_info_existencia_etiquetas
integer x = 2121
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_etiquetas
integer x = 2121
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_etiquetas
integer x = 2121
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_etiquetas
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_etiquetas
integer width = 2990
string text = "Existencia Por Frigoríficos / Etiqueta"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_etiquetas
string tag = "Imprimir Reporte"
integer x = 3387
integer y = 1556
integer taborder = 170
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila,Consolprod, li_varirotula, etiqueta
String	texto_desde, texto_hasta, texto_fecha, ls_productor, ls_planta
Date		ld_desde, ld_hasta

istr_info.titulo		= 'INFORME DE EXISTENCIAS POR ETIQUETA'

OpenWithParm(vinf, istr_info)


IF istr_mant.argumento[9]	=	"1" THEN
	vinf.dw_1.DataObject = "dw_info_existencia_etiquetas"
ELSEIF cbx_2.Checked THEN
	    vinf.dw_1.DataObject = "dw_info_existencia_frigos_etiq_prd_embj"
	 ELSE
	    vinf.dw_1.DataObject = "dw_info_existencia_frigos_etiquetas"
END IF

IF cbx_planta.Checked THEN
	ls_planta	=	"Todas"
ELSE
	IF cbx_consolplan.Checked THEN
		ls_planta	= "Consolidado"
	ELSE	
	   ls_planta	=	is_NomPlanta
	END IF	
END IF

IF cbx_productor.Checked THEN
	ls_productor	=	"Todos"
ELSE
	IF cbx_consolprod.Checked THEN
		ls_productor	= "Consolidado"
	ELSE	
	   ls_productor	=	is_NomProductor
	END IF	
END IF

IF cbx_1.Checked THEN
	etiqueta = -9
ELSE
	etiqueta = -1
END IF	
	
/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF
/*
Variedad
*/
IF IsNull(uo_selvariedad.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF

vinf.dw_1.SetTransObject(sqlca)

ld_desde			=	Date(istr_mant.argumento[23])
ld_hasta			=	Date(istr_mant.argumento[24])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF	

fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[6]), &
			uo_selespecie.Codigo,uo_selvariedad.Codigo,Long(istr_mant.argumento[4]), &
		   istr_mant.argumento[5],date(istr_mant.argumento[23]),date(istr_mant.argumento[24]), & 
			Integer(istr_mant.argumento[27]),Integer(istr_mant.argumento[28]),ii_control,&
			Integer(istr_mant.argumento[35]),li_varirotula,etiqueta)
			
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify('planta.text = "' + ls_planta + '"')
	vinf.dw_1.Modify('control_d.text = "' + is_control_d + '"')
	vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
	vinf.dw_1.Modify("Productor.text = '" + ls_productor + "'")
	vinf.dw_1.Modify("status.text = '" + istr_mant.argumento[36] + "'")

	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF

END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_etiquetas
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3387
integer y = 1864
integer taborder = 180
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_cliente from datawindow within w_info_existencia_etiquetas
integer x = 631
integer y = 524
integer width = 1207
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	data

IF NoExisteCliente(istr_mant.argumento[1]) THEN
	dw_cliente.SetItem(1, "clie_codigo", gi_codexport)
	dw_cliente.SetFocus()
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_existencia_etiquetas
integer x = 288
integer y = 524
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_existencia_etiquetas
integer x = 247
integer y = 440
integer width = 2990
integer height = 204
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

type st_especie from statictext within w_info_existencia_etiquetas
integer x = 288
integer y = 780
integer width = 238
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Especie"
boolean focusrectangle = false
end type

type st_nro2 from statictext within w_info_existencia_etiquetas
integer x = 247
integer y = 648
integer width = 2990
integer height = 288
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

type st_variedad from statictext within w_info_existencia_etiquetas
integer x = 1851
integer y = 780
integer width = 279
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type em_embalaje from editmask within w_info_existencia_etiquetas
integer x = 2711
integer y = 1060
integer width = 297
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
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[5]	=	ls_embalaje
END IF
end event

type st_embalaje from statictext within w_info_existencia_etiquetas
integer x = 2322
integer y = 1072
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
long backcolor = 33543637
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_existencia_etiquetas
integer x = 2711
integer y = 952
integer width = 402
integer height = 80
integer taborder = 120
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
	istr_mant.argumento[5]		=	'Z'
ELSE
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_existencia_etiquetas
integer x = 3026
integer y = 1068
integer width = 96
integer height = 84
integer taborder = 140
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

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[5]	=	lstr_busq.argum[2]
END IF
end event

type st_5 from statictext within w_info_existencia_etiquetas
integer x = 247
integer y = 940
integer width = 2990
integer height = 348
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

type st_productor from statictext within w_info_existencia_etiquetas
integer x = 288
integer y = 1036
integer width = 297
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Productor"
boolean focusrectangle = false
end type

type em_productor from editmask within w_info_existencia_etiquetas
integer x = 631
integer y = 1036
integer width = 297
integer height = 96
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
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;String		ls_Nombre
Long			ll_productor
Integer		li_cliente

IF This.Text <> '' AND This.Text <> '0' THEN
	ll_productor	=	Long(This.Text)
	li_cliente 		=  dw_cliente.Object.clie_codigo[1]
	
	SELECT	prod_nombre
		INTO	:is_NomProductor
		FROM	dba.productores as pro,dba.productoresclientes as cli
		WHERE	pro.prod_codigo =	:ll_productor
		AND	pro.prod_codigo = cli.prod_codigo
		AND	:li_cliente in (-1,cli.clie_codigo);
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla Productores")
		This.SetFocus()
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de productor no ha sido definido o no existe para este cliente.~r~r" + &
			"Ingrese o seleccione otro Código.")
		em_productor.Text 	= ''
		sle_productor.Text	= ''
		This.SetFocus()
	ELSE
		sle_productor.Text		=	is_NomProductor//ls_nombre
		istr_mant.argumento[4]	=	String(ll_productor)
		istr_mant.argumento[34]	=	is_NomProductor//ls_nombre
	END IF
END IF	

end event

type cb_buscaproductor from commandbutton within w_info_existencia_etiquetas
integer x = 937
integer y = 1044
integer width = 96
integer height = 84
integer taborder = 110
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

OpenWithParm(w_busc_productores_clientes, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[3] = "" THEN
	em_productor.SetFocus()
ELSE
	em_productor.Text			=	lstr_busq.argum[3]
	sle_productor.Text		=	lstr_busq.argum[4]
	istr_mant.argumento[4]	=	lstr_busq.argum[3]
	istr_mant.argumento[34]	=	lstr_busq.argum[4]
END IF
end event

type sle_productor from singlelineedit within w_info_existencia_etiquetas
integer x = 1042
integer y = 1036
integer width = 1010
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
borderstyle borderstyle = stylelowered!
end type

type cbx_productor from checkbox within w_info_existencia_etiquetas
integer x = 631
integer y = 952
integer width = 347
integer height = 80
integer taborder = 90
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

event clicked;istr_mant.argumento[27] =  "" //Consolidado por productor
IF cbx_productor.Checked THEN
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
	istr_mant.argumento[4]		=	'0'
	istr_mant.argumento[34]		=	'Todos'
	cbx_Consolprod.Checked		=  False
ELSE
	IF NOT cbx_productor.Checked AND NOT cbx_Consolprod.Checked THEN
		em_productor.Enabled			=	True
		cb_buscaproductor.Enabled	=	True
	END IF
END IF
end event

type cbx_planta from checkbox within w_info_existencia_etiquetas
integer x = 2144
integer y = 448
integer width = 402
integer height = 80
integer taborder = 30
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

event clicked;call super::clicked;
istr_mant.argumento[28] =  "" //Consolidado por planta
IF cbx_planta.Checked THEN
	dw_planta.Enabled			=	False
	cbx_Consolplan.Checked	=  False
	istr_mant.argumento[6]	=	'0'
ELSE
	IF NOT cbx_planta.Checked AND NOT cbx_Consolplan.Checked THEN
			 dw_planta.Enabled		=	True
		    istr_mant.argumento[6]	=	String(dw_planta.GetItemNumber(1,"plde_codigo"))
			 is_NomPlanta				=	idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")
   END IF
END IF	

end event

type dw_planta from datawindow within w_info_existencia_etiquetas
integer x = 2144
integer y = 524
integer width = 969
integer height = 96
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[6]	=	data
IF NoExistePlanta(istr_mant.argumento[6]) THEN
	dw_planta.SetItem(1, "plde_codigo", gi_codplanta)
	dw_planta.SetFocus()
	RETURN 1
END IF




end event

event itemerror;RETURN 1
end event

type st_planta from statictext within w_info_existencia_etiquetas
integer x = 1851
integer y = 540
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Planta"
boolean focusrectangle = false
end type

type st_55 from statictext within w_info_existencia_etiquetas
integer x = 247
integer y = 1288
integer width = 2990
integer height = 168
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

type st_2 from statictext within w_info_existencia_etiquetas
integer x = 805
integer y = 1332
integer width = 425
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_existencia_etiquetas
integer x = 1179
integer y = 1320
integer width = 393
integer height = 96
integer taborder = 150
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

event modified;istr_mant.argumento[23]	=	This.Text
end event

type st_7 from statictext within w_info_existencia_etiquetas
integer x = 1655
integer y = 1336
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_existencia_etiquetas
integer x = 1989
integer y = 1320
integer width = 393
integer height = 96
integer taborder = 160
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

event modified;istr_mant.argumento[24]	=	This.Text
end event

type rb_1 from radiobutton within w_info_existencia_etiquetas
integer x = 969
integer y = 1752
integer width = 457
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Individual"
boolean checked = true
end type

event clicked;istr_mant.argumento[9]	=	"1"

IF This.Checked THEN
	cbx_1.Enabled = False
	cbx_1.Checked = False
	cbx_2.Enabled = False
	cbx_2.Checked = False

	
END IF	
	
end event

type rb_2 from radiobutton within w_info_existencia_etiquetas
integer x = 969
integer y = 1904
integer width = 439
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Columnas"
end type

event clicked;call super::clicked;istr_mant.argumento[9]	=	"2"

IF This.Checked THEN
	cbx_1.Enabled = True
	cbx_2.Enabled = True

END IF	
	
end event

type cbx_consolprod from checkbox within w_info_existencia_etiquetas
integer x = 1042
integer y = 952
integer width = 517
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
string text = "Consolidado"
end type

event clicked;istr_mant.argumento[27] =  '0' //Consolidado por productor
IF cbx_consolprod.Checked THEN
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
	istr_mant.argumento[4]		=	'0'
	istr_mant.argumento[34]		=	'Todos'
	cbx_productor.Checked		=  False
	istr_mant.argumento[27] 	=  '1'
ELSE
	em_productor.Enabled			=	True
	cb_buscaproductor.Enabled	=	True
END IF
end event

type cbx_consolplan from checkbox within w_info_existencia_etiquetas
integer x = 2565
integer y = 460
integer width = 471
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Consolidado"
end type

event clicked;istr_mant.argumento[28] =  '0' //Consolidado por planta
IF cbx_Consolplan.Checked THEN
	dw_planta.Enabled			=	False
	cbx_planta.Checked      =  False
	istr_mant.argumento[6]	=	'0'
	istr_mant.argumento[28] =  '1' 
ELSE
   dw_planta.Enabled			=	True
   istr_mant.argumento[6]	=	String(dw_planta.GetItemNumber(1,"plde_codigo"))
   is_NomPlanta				=	idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")

END IF	


end event

type rb_controltodos from radiobutton within w_info_existencia_etiquetas
integer x = 471
integer y = 1536
integer width = 571
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked=True THEN
	ii_control = -1
	is_control_d = this.text
end if
end event

type rb_rechazados from radiobutton within w_info_existencia_etiquetas
integer x = 969
integer y = 1536
integer width = 571
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
string text = "Rechazados"
end type

event clicked;IF This.checked=True THEN
	ii_control=3
	is_control_d = this.Text
END IF
end event

type rb_objetados from radiobutton within w_info_existencia_etiquetas
integer x = 1637
integer y = 1536
integer width = 571
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
string text = "Objetados"
end type

event clicked;IF This.checked=True THEN
	ii_control=2
	is_control_d = this.Text
END IF
end event

type rb_habilitado from radiobutton within w_info_existencia_etiquetas
integer x = 2363
integer y = 1536
integer width = 571
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
string text = "Habilitados"
end type

event clicked;IF This.checked=True THEN
	ii_control=1
	is_control_d = this.Text
END IF
end event

type gb_6 from groupbox within w_info_existencia_etiquetas
integer x = 302
integer y = 1460
integer width = 2862
integer height = 180
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Control Calidad"
end type

type st_1 from statictext within w_info_existencia_etiquetas
integer x = 247
integer y = 1456
integer width = 2990
integer height = 216
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

type st_3 from statictext within w_info_existencia_etiquetas
integer x = 288
integer y = 1192
integer width = 265
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Status"
boolean focusrectangle = false
end type

type cbx_status from checkbox within w_info_existencia_etiquetas
integer x = 631
integer y = 1192
integer width = 544
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
string text = "Consolidados    "
end type

event clicked;IF This.Checked THEN
   istr_mant.argumento[35]   =	'1'							// status
   istr_mant.argumento[36]	=  "Consolidados "			// status nombre
ELSE
   istr_mant.argumento[35]   =	'0'							// status
   istr_mant.argumento[36]	=  "Todos "						// status nombre
END IF	
//IF This.Checked THEN
//	dw_stat.Enabled			=	False
//	istr_mant.argumento[6]   =	'0'
//	istr_mant.argumento[7]	=  "Todos"	
//	dw_stat.Reset()
//	dw_stat.InsertRow(0)
//	dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(166,180,210)
//	
//ELSE
//	dw_stat.Enabled			=	True	
//	dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(255, 255, 255)
//	dw_stat.InsertRow(0)
//	dw_stat.SetItem(1, "stat_codigo", 1)
//	istr_mant.argumento[7]	=  "Normal"
//END IF
end event

type dw_stat from datawindow within w_info_existencia_etiquetas
boolean visible = false
integer x = 1198
integer y = 1156
integer width = 969
integer height = 88
integer taborder = 110
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[6]	=	data
istr_mant.argumento[7]	=  f_statnombre(integer(data))

end event

type uo_selespecie from uo_seleccion_especie within w_info_existencia_etiquetas
event destroy ( )
integer x = 631
integer y = 664
integer height = 180
integer taborder = 240
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
		uo_selvariedad.Enabled						=	False		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
		uo_selvariedad.Enabled						=	True	
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_etiquetas
event destroy ( )
integer x = 2144
integer y = 668
integer taborder = 260
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_existencia_etiquetas
integer x = 2144
integer y = 848
integer width = 800
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
string text = "Variedad Rotulada"
end type

type cbx_1 from checkbox within w_info_existencia_etiquetas
integer x = 1445
integer y = 1904
integer width = 654
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Consolida Etiquetas"
end type

type gb_3 from groupbox within w_info_existencia_etiquetas
integer x = 315
integer y = 1684
integer width = 2862
integer height = 336
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Informe"
end type

type st_14 from statictext within w_info_existencia_etiquetas
integer x = 247
integer y = 1672
integer width = 2990
integer height = 376
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

type cbx_2 from checkbox within w_info_existencia_etiquetas
integer x = 2181
integer y = 1904
integer width = 951
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Orden Productor/Embalaje"
end type

