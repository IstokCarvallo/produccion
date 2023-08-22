$PBExportHeader$w_info_afecto_numerales_general.srw
forward
global type w_info_afecto_numerales_general from w_para_informes
end type
type st_1 from statictext within w_info_afecto_numerales_general
end type
type dw_cliente from datawindow within w_info_afecto_numerales_general
end type
type st_6 from statictext within w_info_afecto_numerales_general
end type
type dw_planta from datawindow within w_info_afecto_numerales_general
end type
type st_3 from statictext within w_info_afecto_numerales_general
end type
type st_8 from statictext within w_info_afecto_numerales_general
end type
type dw_productor from datawindow within w_info_afecto_numerales_general
end type
type cbx_productor from checkbox within w_info_afecto_numerales_general
end type
type st_5 from statictext within w_info_afecto_numerales_general
end type
type st_variedad from statictext within w_info_afecto_numerales_general
end type
type st_14 from statictext within w_info_afecto_numerales_general
end type
type em_dias from editmask within w_info_afecto_numerales_general
end type
type st_15 from statictext within w_info_afecto_numerales_general
end type
type st_fechaembarque from statictext within w_info_afecto_numerales_general
end type
type em_hasta from editmask within w_info_afecto_numerales_general
end type
type em_desde from editmask within w_info_afecto_numerales_general
end type
type st_2 from statictext within w_info_afecto_numerales_general
end type
type uo_selespecie from uo_seleccion_especie within w_info_afecto_numerales_general
end type
type cbx_varirotula from checkbox within w_info_afecto_numerales_general
end type
end forward

global type w_info_afecto_numerales_general from w_para_informes
integer x = 14
integer y = 32
integer width = 2921
integer height = 2196
string title = "Listado Afectos a Cobro de Numerales General"
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
st_8 st_8
dw_productor dw_productor
cbx_productor cbx_productor
st_5 st_5
st_variedad st_variedad
st_14 st_14
em_dias em_dias
st_15 st_15
st_fechaembarque st_fechaembarque
em_hasta em_hasta
em_desde em_desde
st_2 st_2
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
end type
global w_info_afecto_numerales_general w_info_afecto_numerales_general

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente, idwc_planta, idwc_productor

String is_NomPlanta

uo_seleccion_especie		iuo_selespecie
end variables

forward prototypes
public function boolean existepacking (integer li_cliente, ref string ls_columna)
public function boolean existeespecie (integer cliente, integer especie)
public function boolean existeproductor (integer li_cliente, long ll_productor)
end prototypes

public function boolean existepacking (integer li_cliente, ref string ls_columna);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
WHERE	plde_codigo =  :ls_Columna;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[7] = ls_columna	
	RETURN True 
END IF
end function

public function boolean existeespecie (integer cliente, integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dba.especies
	WHERE	espe_codigo	=	:Especie ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Especies")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

public function boolean existeproductor (integer li_cliente, long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:ll_Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_afecto_numerales_general.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.st_8=create st_8
this.dw_productor=create dw_productor
this.cbx_productor=create cbx_productor
this.st_5=create st_5
this.st_variedad=create st_variedad
this.st_14=create st_14
this.em_dias=create em_dias
this.st_15=create st_15
this.st_fechaembarque=create st_fechaembarque
this.em_hasta=create em_hasta
this.em_desde=create em_desde
this.st_2=create st_2
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_8
this.Control[iCurrent+7]=this.dw_productor
this.Control[iCurrent+8]=this.cbx_productor
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_variedad
this.Control[iCurrent+11]=this.st_14
this.Control[iCurrent+12]=this.em_dias
this.Control[iCurrent+13]=this.st_15
this.Control[iCurrent+14]=this.st_fechaembarque
this.Control[iCurrent+15]=this.em_hasta
this.Control[iCurrent+16]=this.em_desde
this.Control[iCurrent+17]=this.st_2
this.Control[iCurrent+18]=this.uo_selespecie
this.Control[iCurrent+19]=this.cbx_varirotula
end on

on w_info_afecto_numerales_general.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.dw_productor)
destroy(this.cbx_productor)
destroy(this.st_5)
destroy(this.st_variedad)
destroy(this.st_14)
destroy(this.em_dias)
destroy(this.st_15)
destroy(this.st_fechaembarque)
destroy(this.em_hasta)
destroy(this.em_desde)
destroy(this.st_2)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
end on

event open;Boolean lb_Cerrar

x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve(gi_CodExport)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve()
dw_productor.InsertRow(0)

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())

istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	String(gi_CodPlanta)		//	planta
istr_mant.argumento[6]  =  "-1"							//	productor
end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_afecto_numerales_general
end type

type st_usuario from w_para_informes`st_usuario within w_info_afecto_numerales_general
end type

type st_temporada from w_para_informes`st_temporada within w_info_afecto_numerales_general
end type

type p_logo from w_para_informes`p_logo within w_info_afecto_numerales_general
end type

type st_titulo from w_para_informes`st_titulo within w_info_afecto_numerales_general
integer width = 2203
string text = "Listado Afectos a Cobro de Numerales General"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_afecto_numerales_general
integer x = 2569
integer y = 1348
integer taborder = 90
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_varirotula
String	ls_dias, ls_fechas
Date		ld_hasta,ld_desde
Long		ll_productor

istr_info.titulo	= 'LISTADO DE PALLETS AFECTOS A NUMERALES'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_afectonumerales_general"

/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
ll_productor	=	Long(istr_mant.argumento[6])

ls_dias			=	String(Integer(em_dias.Text))
ld_hasta			=	Date(em_hasta.Text)
ld_desde			=	Date(em_desde.Text)

ls_fechas      =  "Fecha Ingreso Desde "+String(ld_desde)+" Fecha Tope Hasta "+String(ld_hasta)

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, uo_selespecie.Codigo,&
									 Integer(ls_dias), ll_productor, ld_hasta, ld_desde,li_varirotula)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("Fechaembarque.text = '" + ls_fechas + "'")
		vinf.dw_1.Modify("Dias.text = '" + ls_dias + "'")		
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_afecto_numerales_general
integer x = 2569
integer y = 1636
integer taborder = 100
end type

type st_1 from statictext within w_info_afecto_numerales_general
integer x = 343
integer y = 704
integer width = 462
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
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_afecto_numerales_general
integer x = 1070
integer y = 512
integer width = 1143
integer height = 104
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	idwc_planta.Retrieve(1)
	idwc_productor.Retrieve()	
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_afecto_numerales_general
integer x = 343
integer y = 552
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

type dw_planta from datawindow within w_info_afecto_numerales_general
integer x = 1070
integer y = 688
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null
Integer	li_Cliente

SetNull(ls_null)

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_afecto_numerales_general
integer x = 343
integer y = 1132
integer width = 421
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

type st_8 from statictext within w_info_afecto_numerales_general
integer x = 343
integer y = 932
integer width = 306
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
string text = "Productor"
boolean focusrectangle = false
end type

type dw_productor from datawindow within w_info_afecto_numerales_general
integer x = 1070
integer y = 904
integer width = 1138
integer height = 92
integer taborder = 40
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

event itemchanged;String	 ls_null
SetNull(ls_null)

IF ExisteProductor(Integer(istr_mant.argumento[1]),Long(data)) THEN
	istr_mant.argumento[6]	=	data	
	RETURN 0
ELSE
	This.SetItem(1, "prod_codigo", ls_null)
	RETURN 1
END IF
end event

type cbx_productor from checkbox within w_info_afecto_numerales_general
integer x = 1070
integer y = 816
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF cbx_productor.Checked = TRUE THEN
	dw_productor.Enabled  = False
	dw_productor.Reset()
	dw_productor.insertrow(0)
	istr_mant.argumento[6]	=	'-1'
ELSE
	dw_productor.Enabled  = True
	dw_productor.SetFocus()
	dw_productor.Reset()
	dw_productor.InsertRow(0)
END IF
	
end event

type st_5 from statictext within w_info_afecto_numerales_general
integer x = 247
integer y = 1028
integer width = 2203
integer height = 332
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

type st_variedad from statictext within w_info_afecto_numerales_general
integer x = 343
integer y = 1400
integer width = 453
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Días Frigorífico"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_afecto_numerales_general
integer x = 247
integer y = 440
integer width = 2203
integer height = 588
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

type em_dias from editmask within w_info_afecto_numerales_general
integer x = 1070
integer y = 1400
integer width = 238
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "10"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "##"
boolean spin = true
double increment = 1
string minmax = "1~~99"
end type

type st_15 from statictext within w_info_afecto_numerales_general
integer x = 247
integer y = 1360
integer width = 2203
integer height = 492
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

type st_fechaembarque from statictext within w_info_afecto_numerales_general
integer x = 343
integer y = 1696
integer width = 686
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fecha Tope"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_afecto_numerales_general
integer x = 1070
integer y = 1696
integer width = 393
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
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[4]	=	This.Text
end event

type em_desde from editmask within w_info_afecto_numerales_general
integer x = 1070
integer y = 1548
integer width = 393
integer height = 96
integer taborder = 70
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

event modified;istr_mant.argumento[4]	=	This.Text
end event

type st_2 from statictext within w_info_afecto_numerales_general
integer x = 343
integer y = 1548
integer width = 704
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fecha Ingreso Desde"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_afecto_numerales_general
event destroy ( )
integer x = 1070
integer y = 1044
integer height = 180
integer taborder = 70
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cbx_varirotula from checkbox within w_info_afecto_numerales_general
integer x = 1070
integer y = 1236
integer width = 626
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

