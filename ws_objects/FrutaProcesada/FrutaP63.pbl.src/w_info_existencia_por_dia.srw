$PBExportHeader$w_info_existencia_por_dia.srw
forward
global type w_info_existencia_por_dia from w_para_informes
end type
type st_1 from statictext within w_info_existencia_por_dia
end type
type dw_cliente from datawindow within w_info_existencia_por_dia
end type
type st_6 from statictext within w_info_existencia_por_dia
end type
type dw_planta from datawindow within w_info_existencia_por_dia
end type
type st_3 from statictext within w_info_existencia_por_dia
end type
type st_variedad from statictext within w_info_existencia_por_dia
end type
type st_4 from statictext within w_info_existencia_por_dia
end type
type st_productor from statictext within w_info_existencia_por_dia
end type
type cbx_productor from checkbox within w_info_existencia_por_dia
end type
type em_productor from editmask within w_info_existencia_por_dia
end type
type cb_buscaproductor from commandbutton within w_info_existencia_por_dia
end type
type sle_productor from singlelineedit within w_info_existencia_por_dia
end type
type st_2 from statictext within w_info_existencia_por_dia
end type
type em_hasta from editmask within w_info_existencia_por_dia
end type
type cbx_planta from checkbox within w_info_existencia_por_dia
end type
type cbx_cliente from checkbox within w_info_existencia_por_dia
end type
type st_embalaje from statictext within w_info_existencia_por_dia
end type
type em_embalaje from editmask within w_info_existencia_por_dia
end type
type cb_buscaembalaje from commandbutton within w_info_existencia_por_dia
end type
type cbx_embalaje from checkbox within w_info_existencia_por_dia
end type
type st_5 from statictext within w_info_existencia_por_dia
end type
type cbx_consclie from checkbox within w_info_existencia_por_dia
end type
type cbx_consplan from checkbox within w_info_existencia_por_dia
end type
type cbx_consprod from checkbox within w_info_existencia_por_dia
end type
type cbx_consemba from checkbox within w_info_existencia_por_dia
end type
type st_55 from statictext within w_info_existencia_por_dia
end type
type st_8 from statictext within w_info_existencia_por_dia
end type
type em_calidad from editmask within w_info_existencia_por_dia
end type
type st_calidad from statictext within w_info_existencia_por_dia
end type
type cbx_calidad from checkbox within w_info_existencia_por_dia
end type
type cbx_conscalidad from checkbox within w_info_existencia_por_dia
end type
type uo_selespecie from uo_seleccion_especie within w_info_existencia_por_dia
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_por_dia
end type
type cbx_varirotula from checkbox within w_info_existencia_por_dia
end type
type em_desde from editmask within w_info_existencia_por_dia
end type
type st_7 from statictext within w_info_existencia_por_dia
end type
type cbx_consfecha from checkbox within w_info_existencia_por_dia
end type
type cbx_conszona from checkbox within w_info_existencia_por_dia
end type
end forward

global type w_info_existencia_por_dia from w_para_informes
integer x = 14
integer y = 32
integer width = 3387
integer height = 2192
string title = "Existencias Por Día"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
st_variedad st_variedad
st_4 st_4
st_productor st_productor
cbx_productor cbx_productor
em_productor em_productor
cb_buscaproductor cb_buscaproductor
sle_productor sle_productor
st_2 st_2
em_hasta em_hasta
cbx_planta cbx_planta
cbx_cliente cbx_cliente
st_embalaje st_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_embalaje cbx_embalaje
st_5 st_5
cbx_consclie cbx_consclie
cbx_consplan cbx_consplan
cbx_consprod cbx_consprod
cbx_consemba cbx_consemba
st_55 st_55
st_8 st_8
em_calidad em_calidad
st_calidad st_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
em_desde em_desde
st_7 st_7
cbx_consfecha cbx_consfecha
cbx_conszona cbx_conszona
end type
global w_info_existencia_por_dia w_info_existencia_por_dia

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente, idwc_planta,idwc_planta2
						
uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad	
uo_calibre					iuo_calibre
end variables

on w_info_existencia_por_dia.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_4=create st_4
this.st_productor=create st_productor
this.cbx_productor=create cbx_productor
this.em_productor=create em_productor
this.cb_buscaproductor=create cb_buscaproductor
this.sle_productor=create sle_productor
this.st_2=create st_2
this.em_hasta=create em_hasta
this.cbx_planta=create cbx_planta
this.cbx_cliente=create cbx_cliente
this.st_embalaje=create st_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_embalaje=create cbx_embalaje
this.st_5=create st_5
this.cbx_consclie=create cbx_consclie
this.cbx_consplan=create cbx_consplan
this.cbx_consprod=create cbx_consprod
this.cbx_consemba=create cbx_consemba
this.st_55=create st_55
this.st_8=create st_8
this.em_calidad=create em_calidad
this.st_calidad=create st_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.em_desde=create em_desde
this.st_7=create st_7
this.cbx_consfecha=create cbx_consfecha
this.cbx_conszona=create cbx_conszona
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.st_productor
this.Control[iCurrent+9]=this.cbx_productor
this.Control[iCurrent+10]=this.em_productor
this.Control[iCurrent+11]=this.cb_buscaproductor
this.Control[iCurrent+12]=this.sle_productor
this.Control[iCurrent+13]=this.st_2
this.Control[iCurrent+14]=this.em_hasta
this.Control[iCurrent+15]=this.cbx_planta
this.Control[iCurrent+16]=this.cbx_cliente
this.Control[iCurrent+17]=this.st_embalaje
this.Control[iCurrent+18]=this.em_embalaje
this.Control[iCurrent+19]=this.cb_buscaembalaje
this.Control[iCurrent+20]=this.cbx_embalaje
this.Control[iCurrent+21]=this.st_5
this.Control[iCurrent+22]=this.cbx_consclie
this.Control[iCurrent+23]=this.cbx_consplan
this.Control[iCurrent+24]=this.cbx_consprod
this.Control[iCurrent+25]=this.cbx_consemba
this.Control[iCurrent+26]=this.st_55
this.Control[iCurrent+27]=this.st_8
this.Control[iCurrent+28]=this.em_calidad
this.Control[iCurrent+29]=this.st_calidad
this.Control[iCurrent+30]=this.cbx_calidad
this.Control[iCurrent+31]=this.cbx_conscalidad
this.Control[iCurrent+32]=this.uo_selespecie
this.Control[iCurrent+33]=this.uo_selvariedad
this.Control[iCurrent+34]=this.cbx_varirotula
this.Control[iCurrent+35]=this.em_desde
this.Control[iCurrent+36]=this.st_7
this.Control[iCurrent+37]=this.cbx_consfecha
this.Control[iCurrent+38]=this.cbx_conszona
end on

on w_info_existencia_por_dia.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_4)
destroy(this.st_productor)
destroy(this.cbx_productor)
destroy(this.em_productor)
destroy(this.cb_buscaproductor)
destroy(this.sle_productor)
destroy(this.st_2)
destroy(this.em_hasta)
destroy(this.cbx_planta)
destroy(this.cbx_cliente)
destroy(this.st_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_embalaje)
destroy(this.st_5)
destroy(this.cbx_consclie)
destroy(this.cbx_consplan)
destroy(this.cbx_consprod)
destroy(this.cbx_consemba)
destroy(this.st_55)
destroy(this.st_8)
destroy(this.em_calidad)
destroy(this.st_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.cbx_consfecha)
destroy(this.cbx_conszona)
end on

event open;call super::open;/* Argumentos :
					 [1] = Cliente
					 [2] = Planta
					 [3] = Especie
					 [4] = Variedad
					 [5] = Calibre
*/

Boolean lb_Cerrar

x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)

iuo_calibre   						=	Create uo_calibre

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

// uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
END IF

//dw_especie.GetChild("espe_codigo", idwc_especie)
//idwc_especie.SetTransObject(sqlca)
//idwc_especie.Retrieve()
//dw_especie.InsertRow(0)
//dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())

istr_mant.argumento[1]	= 	String(gi_CodExport)
istr_mant.argumento[2]	= 	String(gi_CodPlanta)
istr_mant.argumento[3]	=	String(gi_CodEspecie)
istr_mant.argumento[4]	=	'0'
istr_mant.argumento[5]	=	'0'
istr_mant.argumento[6]	=	'Consolidados'
istr_mant.argumento[23]	= 	em_desde.Text
istr_mant.argumento[24]	=	em_hasta.Text
end event

type pb_excel from w_para_informes`pb_excel within w_info_existencia_por_dia
integer x = 2967
integer y = 928
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_por_dia
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_por_dia
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_por_dia
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_por_dia
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_por_dia
integer width = 2523
string text = "Informe de Existencia Por Día"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_por_dia
string tag = "Imprimir Reporte"
integer x = 2976
integer y = 1352
integer taborder = 120
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cbx_especie, li_planta, li_cliente, li_varirotula, li_pallet, li_fecha, li_zona
Date		ld_desde, ld_hasta
Long		ll_productor
String	ls_Embalaje, ls_calibre

istr_info.titulo	= 'INFORME DE EXISTENCIAS POR DIAS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_existencia_por_dia"
vinf.dw_1.SetTransObject(sqlca)

IF cbx_ConsClie.Checked THEN
	li_cliente   =  -9
ELSEIF cbx_cliente.Checked THEN
	li_cliente   =  -1
ELSE
   li_cliente	=	dw_cliente.Object.clie_codigo[1]
END IF

IF cbx_consfecha.Checked THEN
	li_fecha   =  -9
ELSE
	li_fecha   =  -1
END IF

IF cbx_conszona.Checked THEN
	li_zona = -9
ELSE
	li_zona = -1
END IF	

IF cbx_ConsPlan.Checked THEN
	li_Planta   =  -9
ELSEIF cbx_planta.Checked THEN
	li_Planta   =  -1
ELSE
   li_planta	=	dw_planta.Object.plde_codigo[1]
END IF

IF cbx_ConsProd.Checked THEN
	ll_productor   =  -9
ELSEIF cbx_productor.Checked THEN
	ll_productor   =  -1
ELSE
   ll_productor	=	Long(em_Productor.Text)
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

IF cbx_ConsEmba.Checked THEN
	ls_Embalaje   =  '**'
ELSEIF cbx_embalaje.Checked THEN
	ls_Embalaje   =  '*'
ELSE
   ls_Embalaje	=	Trim(em_embalaje.Text)
END IF

IF cbx_conscalidad.Checked THEN
	ls_calibre = '**'
ELSEIF cbx_calidad.Checked THEN
	ls_calibre   =  '*'
ELSE
	ls_calibre = 	istr_mant.argumento[5]
END IF

//ld_desde			=	Date(em_desde.Text)
ld_desde			=	Date(em_Hasta.Text)
ld_hasta			=	Date(em_Hasta.Text)

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

fila	=	vinf.dw_1.Retrieve(ld_desde,ld_hasta,li_cliente, li_planta, uo_selespecie.Codigo,&
									 uo_selvariedad.Codigo,ls_Embalaje,ll_productor,ls_calibre, li_varirotula,li_pallet,li_fecha,li_zona)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				   "de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)
ELSE
	   F_Membrete(vinf.dw_1)
	
		vinf.dw_1.Modify("desde.text = '" + String(ld_desde,'dd/mm/yyyy')+"'")
		vinf.dw_1.Modify("hasta.text = '" + String(ld_hasta,'dd/mm/yyyy')+"'")	
		IF li_cliente = -9 AND uo_selvariedad.Codigo = -9 AND ls_Embalaje = '**' &
			AND ll_productor = -9 THEN
			vinf.dw_1.Modify('DataWindow.Trailer.1.height=0 ')
		END IF
		//vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_por_dia
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2976
integer y = 1684
integer taborder = 130
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_existencia_por_dia
integer x = 347
integer y = 640
integer width = 215
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

type dw_cliente from datawindow within w_info_existencia_por_dia
integer x = 686
integer y = 496
integer width = 1147
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_existencia_por_dia
integer x = 347
integer y = 504
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

type dw_planta from datawindow within w_info_existencia_por_dia
integer x = 686
integer y = 628
integer width = 965
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_existencia_por_dia
integer x = 347
integer y = 1100
integer width = 251
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
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_existencia_por_dia
integer x = 347
integer y = 1292
integer width = 279
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

type st_4 from statictext within w_info_existencia_por_dia
integer x = 251
integer y = 440
integer width = 2523
integer height = 996
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_productor from statictext within w_info_existencia_por_dia
integer x = 347
integer y = 764
integer width = 297
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
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_productor from checkbox within w_info_existencia_por_dia
integer x = 1865
integer y = 772
integer width = 279
integer height = 80
integer taborder = 30
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
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
ELSE
	em_productor.Enabled			=	True
	cb_buscaproductor.Enabled	=	True
END IF
end event

type em_productor from editmask within w_info_existencia_por_dia
integer x = 686
integer y = 760
integer width = 233
integer height = 96
integer taborder = 40
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
	
	IF cbx_cliente.Checked OR cbx_consclie.Checked THEN
		li_cliente = -1
	ELSE
		li_cliente 		=  dw_cliente.Object.clie_codigo[1]
	END IF
	
	SELECT	prod_nombre
		INTO	:ls_Nombre
		FROM	dbo.productores as pro,dbo.productoresclientes as cli
		WHERE	pro.prod_codigo =	:ll_productor
		AND	pro.prod_codigo = cli.prod_codigo
		AND	:li_cliente in (-1,cli.clie_codigo);
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla Productores")
		This.SetFocus()
	ELSEIF sqlca.SQLCode = 100 THEN
	
		MessageBox("Atención", "Código de productor no ha sido definido o pertenece a otro cliente.~r~r" + &
			"Ingrese o seleccione otro Código.")
		em_productor.Text 	= ''
		sle_productor.Text 	= ''
		This.SetFocus()
	ELSE
		sle_productor.Text		=	ls_Nombre
		istr_mant.argumento[4]	=	String(ll_productor)
	END IF
END IF	
end event

type cb_buscaproductor from commandbutton within w_info_existencia_por_dia
integer x = 937
integer y = 772
integer width = 96
integer height = 84
integer taborder = 50
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

IF cbx_cliente.Checked OR cbx_consclie.Checked THEN
	lstr_busq.argum[1]	=	'-1'//istr_mant.argumento[1]
ELSE
	lstr_busq.argum[1]	=	istr_mant.argumento[1]
END IF

 // Cliente

OpenWithParm(w_busc_productores_clientes, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[3] = "" THEN
	em_productor.SetFocus()
ELSE
	em_productor.Text			=	lstr_busq.argum[3]
	sle_productor.Text		=	lstr_busq.argum[4]
	istr_mant.argumento[5]	=	lstr_busq.argum[3]
	istr_mant.argumento[6]	=	lstr_busq.argum[4]
END IF
end event

type sle_productor from singlelineedit within w_info_existencia_por_dia
integer x = 686
integer y = 892
integer width = 1147
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_info_existencia_por_dia
integer x = 347
integer y = 1804
integer width = 896
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
string text = "Existencia que hubo el dia ...  "
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_existencia_por_dia
integer x = 1865
integer y = 1788
integer width = 393
integer height = 96
integer taborder = 110
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

event modified;istr_mant.argumento[24]	=	This.Text
end event

type cbx_planta from checkbox within w_info_existencia_por_dia
integer x = 1865
integer y = 636
integer width = 279
integer height = 80
integer taborder = 70
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
end type

event clicked;Integer	li_planta

SetNull(li_planta)
IF This.Checked THEN
	dw_planta.Enabled				=	False
	dw_planta.SetItem(1,"plde_codigo",li_planta)
	istr_mant.argumento[2]		=	'-1'
ELSE
	dw_planta.Enabled				=	True
	dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)
   istr_mant.argumento[2]	= 	String(gi_CodPlanta)
END IF
end event

type cbx_cliente from checkbox within w_info_existencia_por_dia
integer x = 1865
integer y = 504
integer width = 279
integer height = 80
integer taborder = 70
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
end type

event clicked;Integer	li_Cliente

SetNull(li_Cliente)
IF This.Checked THEN
	dw_cliente.Enabled				=	False
	dw_cliente.SetItem(1,"clie_codigo",li_Cliente)
ELSE
	dw_cliente.Enabled				=	True
	dw_cliente.SetItem(1, "clie_codigo", gi_codExport)
END IF
end event

type st_embalaje from statictext within w_info_existencia_por_dia
integer x = 347
integer y = 1640
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

type em_embalaje from editmask within w_info_existencia_por_dia
integer x = 686
integer y = 1628
integer width = 297
integer height = 96
integer taborder = 230
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

li_cliente	=	dw_cliente.Object.clie_codigo[1] // Cliente
ls_embalaje	=	Trim(This.Text)

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dbo.embalajesprod
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
	istr_mant.argumento[8]	=	ls_embalaje
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_existencia_por_dia
integer x = 1015
integer y = 1636
integer width = 96
integer height = 84
integer taborder = 240
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
END IF
end event

type cbx_embalaje from checkbox within w_info_existencia_por_dia
integer x = 1865
integer y = 1636
integer width = 283
integer height = 80
integer taborder = 200
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
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''	
ELSE
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
	
END IF
end event

type st_5 from statictext within w_info_existencia_por_dia
integer x = 251
integer y = 1596
integer width = 2523
integer height = 160
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_consclie from checkbox within w_info_existencia_por_dia
integer x = 2185
integer y = 504
integer width = 503
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;Integer	li_Cliente

SetNull(li_Cliente)

IF This.Checked THEN
	dw_cliente.Enabled	=	False
	dw_cliente.SetItem(1,"clie_codigo",li_Cliente)
	cbx_cliente.Checked	= True
	cbx_cliente.Enabled	=	False
ELSE
	cbx_cliente.Enabled	=	True
END IF
end event

type cbx_consplan from checkbox within w_info_existencia_por_dia
integer x = 2185
integer y = 636
integer width = 507
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;Integer	li_Planta

SetNull(li_Planta)

IF This.Checked THEN
	dw_planta.Enabled	=	False
	dw_planta.SetItem(1,"plde_codigo",li_Planta)
	cbx_planta.Checked	= True
	cbx_planta.Enabled	=	False
ELSE
	cbx_planta.Enabled	=	True
END IF
end event

type cbx_consprod from checkbox within w_info_existencia_por_dia
integer x = 2185
integer y = 772
integer width = 503
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;Integer	li_Cliente

SetNull(li_Cliente)

IF This.Checked THEN
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
	
	cbx_productor.Checked	= True
	cbx_productor.Enabled	=	False
ELSE
	cbx_productor.Enabled	=	True
END IF
end event

type cbx_consemba from checkbox within w_info_existencia_por_dia
integer x = 2185
integer y = 1640
integer width = 544
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;Integer	li_Cliente

SetNull(li_Cliente)

IF This.Checked THEN
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''	
	
	cbx_embalaje.Checked	= True
	cbx_embalaje.Enabled	=	False
ELSE
	cbx_embalaje.Enabled	=	True
END IF
end event

type st_55 from statictext within w_info_existencia_por_dia
integer x = 251
integer y = 1756
integer width = 2523
integer height = 160
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_existencia_por_dia
integer x = 251
integer y = 1436
integer width = 2523
integer height = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_existencia_por_dia
integer x = 686
integer y = 1468
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
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;Integer	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[5]	=	iuo_calibre.calibre
		em_calidad.Text			=	iuo_calibre.calibre
		Return 1
	END IF	
END IF	

end event

type st_calidad from statictext within w_info_existencia_por_dia
integer x = 329
integer y = 1480
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

type cbx_calidad from checkbox within w_info_existencia_por_dia
integer x = 1865
integer y = 1476
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
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type cbx_conscalidad from checkbox within w_info_existencia_por_dia
integer x = 2185
integer y = 1476
integer width = 526
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
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	cbx_calidad.Enabled	=	False
	cbx_calidad.Checked	=	True
ELSE
	cbx_calidad.Enabled	=	True
END IF

end event

type uo_selespecie from uo_seleccion_especie within w_info_existencia_por_dia
event destroy ( )
integer x = 686
integer y = 1016
integer height = 180
integer taborder = 70
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

type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_por_dia
event destroy ( )
integer x = 686
integer y = 1200
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_existencia_por_dia
integer x = 1856
integer y = 1300
integer width = 654
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

type em_desde from editmask within w_info_existencia_por_dia
boolean visible = false
integer x = 686
integer y = 1812
integer width = 393
integer height = 96
integer taborder = 100
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

type st_7 from statictext within w_info_existencia_por_dia
boolean visible = false
integer x = 1125
integer y = 1828
integer width = 297
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Término"
boolean focusrectangle = false
end type

type cbx_consfecha from checkbox within w_info_existencia_por_dia
integer x = 1856
integer y = 1108
integer width = 896
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
string text = "Consolida Fecha Embalaje"
boolean checked = true
end type

type cbx_conszona from checkbox within w_info_existencia_por_dia
integer x = 2185
integer y = 908
integer width = 567
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolida Zona  "
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
ELSE
	em_productor.Enabled			=	True
	cb_buscaproductor.Enabled	=	True
END IF
end event

