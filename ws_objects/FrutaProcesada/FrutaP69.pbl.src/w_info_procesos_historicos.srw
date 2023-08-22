$PBExportHeader$w_info_procesos_historicos.srw
forward
global type w_info_procesos_historicos from w_para_informes
end type
type st_1 from statictext within w_info_procesos_historicos
end type
type st_2 from statictext within w_info_procesos_historicos
end type
type em_desde from editmask within w_info_procesos_historicos
end type
type dw_cliente from datawindow within w_info_procesos_historicos
end type
type st_6 from statictext within w_info_procesos_historicos
end type
type dw_planta from datawindow within w_info_procesos_historicos
end type
type st_3 from statictext within w_info_procesos_historicos
end type
type cbx_planta from checkbox within w_info_procesos_historicos
end type
type st_7 from statictext within w_info_procesos_historicos
end type
type em_hasta from editmask within w_info_procesos_historicos
end type
type st_8 from statictext within w_info_procesos_historicos
end type
type dw_productor from datawindow within w_info_procesos_historicos
end type
type cbx_productor from checkbox within w_info_procesos_historicos
end type
type gb_3 from groupbox within w_info_procesos_historicos
end type
type st_5 from statictext within w_info_procesos_historicos
end type
type cbx_1 from checkbox within w_info_procesos_historicos
end type
type dw_pesoneto from datawindow within w_info_procesos_historicos
end type
type tit_peso from statictext within w_info_procesos_historicos
end type
type st_variedad from statictext within w_info_procesos_historicos
end type
type st_14 from statictext within w_info_procesos_historicos
end type
type gb_4 from groupbox within w_info_procesos_historicos
end type
type st_4 from statictext within w_info_procesos_historicos
end type
type rb_1 from radiobutton within w_info_procesos_historicos
end type
type rb_2 from radiobutton within w_info_procesos_historicos
end type
type rb_4 from radiobutton within w_info_procesos_historicos
end type
type gb_54 from groupbox within w_info_procesos_historicos
end type
type st_54 from statictext within w_info_procesos_historicos
end type
type rb_54 from radiobutton within w_info_procesos_historicos
end type
type rb_53 from radiobutton within w_info_procesos_historicos
end type
type uo_selespecie from uo_seleccion_especie within w_info_procesos_historicos
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_procesos_historicos
end type
type cbx_varirotula from checkbox within w_info_procesos_historicos
end type
end forward

global type w_info_procesos_historicos from w_para_informes
integer x = 14
integer y = 32
integer width = 2939
integer height = 2396
string title = "Peso Equivalente"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
cbx_planta cbx_planta
st_7 st_7
em_hasta em_hasta
st_8 st_8
dw_productor dw_productor
cbx_productor cbx_productor
gb_3 gb_3
st_5 st_5
cbx_1 cbx_1
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_variedad st_variedad
st_14 st_14
gb_4 gb_4
st_4 st_4
rb_1 rb_1
rb_2 rb_2
rb_4 rb_4
gb_54 gb_54
st_54 st_54
rb_54 rb_54
rb_53 rb_53
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
end type
global w_info_procesos_historicos w_info_procesos_historicos

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor, idwc_packing, idwc_pesoneto

String is_NomPlanta

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
end variables

forward prototypes
public function boolean existepacking (integer li_cliente, ref string ls_columna)
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

public function boolean existeproductor (integer li_cliente, long ll_productor);String	ls_Nombre

li_cliente	= dw_cliente.Object.clie_Codigo[1]

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores as pro,dba.productoresclientes as cli
	WHERE	pro.prod_codigo =	:ll_productor
	AND	pro.prod_codigo = cli.prod_codigo
	AND	:li_cliente in (-1,cli.clie_codigo);
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de productor no ha sido definido o pertenece a otro cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_procesos_historicos.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.cbx_planta=create cbx_planta
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.dw_productor=create dw_productor
this.cbx_productor=create cbx_productor
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_1=create cbx_1
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.st_14=create st_14
this.gb_4=create gb_4
this.st_4=create st_4
this.rb_1=create rb_1
this.rb_2=create rb_2
this.rb_4=create rb_4
this.gb_54=create gb_54
this.st_54=create st_54
this.rb_54=create rb_54
this.rb_53=create rb_53
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.dw_cliente
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_planta
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.cbx_planta
this.Control[iCurrent+9]=this.st_7
this.Control[iCurrent+10]=this.em_hasta
this.Control[iCurrent+11]=this.st_8
this.Control[iCurrent+12]=this.dw_productor
this.Control[iCurrent+13]=this.cbx_productor
this.Control[iCurrent+14]=this.gb_3
this.Control[iCurrent+15]=this.st_5
this.Control[iCurrent+16]=this.cbx_1
this.Control[iCurrent+17]=this.dw_pesoneto
this.Control[iCurrent+18]=this.tit_peso
this.Control[iCurrent+19]=this.st_variedad
this.Control[iCurrent+20]=this.st_14
this.Control[iCurrent+21]=this.gb_4
this.Control[iCurrent+22]=this.st_4
this.Control[iCurrent+23]=this.rb_1
this.Control[iCurrent+24]=this.rb_2
this.Control[iCurrent+25]=this.rb_4
this.Control[iCurrent+26]=this.gb_54
this.Control[iCurrent+27]=this.st_54
this.Control[iCurrent+28]=this.rb_54
this.Control[iCurrent+29]=this.rb_53
this.Control[iCurrent+30]=this.uo_selespecie
this.Control[iCurrent+31]=this.uo_selvariedad
this.Control[iCurrent+32]=this.cbx_varirotula
end on

on w_info_procesos_historicos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.cbx_planta)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.dw_productor)
destroy(this.cbx_productor)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_1)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.st_14)
destroy(this.gb_4)
destroy(this.st_4)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.rb_4)
destroy(this.gb_54)
destroy(this.st_54)
destroy(this.rb_54)
destroy(this.rb_53)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
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

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(gi_CodExport)
dw_productor.InsertRow(0)

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve(0)
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 820/100)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

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
END IF

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	"0"							//	planta
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
//istr_mant.argumento[5]	=	String(gi_CodEspecie)	//	especie
istr_mant.argumento[6]  =  "0"								//	productor
istr_mant.argumento[7]  =  "1"							//	procesos
istr_mant.argumento[8]  =  "1"							//	peso
//istr_mant.argumento[9]	= 	"0" 							// variedad
istr_mant.argumento[27]	=	"1"							// Informe por Columnas

end event

type st_computador from w_para_informes`st_computador within w_info_procesos_historicos
end type

type st_usuario from w_para_informes`st_usuario within w_info_procesos_historicos
end type

type st_temporada from w_para_informes`st_temporada within w_info_procesos_historicos
end type

type p_logo from w_para_informes`p_logo within w_info_procesos_historicos
end type

type st_titulo from w_para_informes`st_titulo within w_info_procesos_historicos
integer width = 2203
string text = "Informe Procesos Históricos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_procesos_historicos
integer x = 2569
integer y = 1652
integer taborder = 150
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_especie,li_varirotula
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_cajas

istr_info.titulo	= 'INFORME DE PROCESOS HISTORICOS'
OpenWithParm(vinf, istr_info)

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

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

IF istr_mant.argumento[27]	=	"1" THEN

	IF istr_mant.argumento[7]="1" THEN
		vinf.dw_1.DataObject = "dw_info_inspecciones_historicas"
	ELSEIF istr_mant.argumento[7]="2" THEN
		vinf.dw_1.DataObject = "dw_info_fumigados_historicos"
//	ELSEIF istr_mant.argumento[7]="3" THEN
//		vinf.dw_1.DataObject = "dw_info_repaletizados_historicos"
	ELSE
		vinf.dw_1.DataObject = "dw_info_reetiquetados_historicos"
	END IF
ELSE
	IF istr_mant.argumento[7]="1" THEN
		vinf.dw_1.DataObject = "dw_info_inspecciones_hist_trad"
	ELSEIF istr_mant.argumento[7]="2" THEN
		vinf.dw_1.DataObject = "dw_info_fumigados_hist_trad"
//	ELSEIF istr_mant.argumento[7]="3" THEN
//		vinf.dw_1.DataObject = "dw_info_repaletizados_hist_trad"
	ELSE
		vinf.dw_1.DataObject = "dw_info_reetiquetados_hist_trad"
	END IF
END IF

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
ld_desde			=	Date(istr_mant.argumento[3])
ld_hasta			=	Date(istr_mant.argumento[4])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF cbx_1.Checked=False THEN
	ls_cajas = "Bulto"
	istr_mant.argumento[8]	=	"1"
ELSE
	istr_mant.argumento[8]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas = "Base " + istr_mant.argumento[8] 
END IF

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(Dec(li_cliente), Dec(li_planta),ld_desde, ld_hasta, Dec(uo_selespecie.Codigo),&
									 Dec(istr_mant.argumento[6]),Dec(istr_mant.argumento[8]), &
									 Dec(uo_selvariedad.Codigo),Dec(li_varirotula))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		vinf.dw_1.Modify("Cajas.text = '" + ls_cajas + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_procesos_historicos
integer x = 2569
integer y = 1940
integer taborder = 170
end type

type st_1 from statictext within w_info_procesos_historicos
integer x = 347
integer y = 884
integer width = 462
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

type st_2 from statictext within w_info_procesos_historicos
integer x = 347
integer y = 1676
integer width = 425
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_procesos_historicos
integer x = 791
integer y = 1660
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

event modified;istr_mant.argumento[3]	=	This.Text
end event

type dw_cliente from datawindow within w_info_procesos_historicos
integer x = 791
integer y = 716
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
	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(sqlca)
	idwc_productor.Retrieve(Integer(data))
	dw_productor.InsertRow(0)
	idwc_packing.Retrieve(2)
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_procesos_historicos
integer x = 347
integer y = 724
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

type dw_planta from datawindow within w_info_procesos_historicos
integer x = 791
integer y = 888
integer width = 969
integer height = 92
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
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

type st_3 from statictext within w_info_procesos_historicos
integer x = 347
integer y = 1280
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

type cbx_planta from checkbox within w_info_procesos_historicos
integer x = 791
integer y = 812
integer width = 402
integer height = 76
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

event clicked;IF This.Checked THEN
	dw_planta.Enabled			=	False
	istr_mant.argumento[2]	=	'0'
ELSE
	dw_planta.Enabled			=	True
	dw_planta.SetFocus()
END IF
end event

type st_7 from statictext within w_info_procesos_historicos
integer x = 1225
integer y = 1676
integer width = 297
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

type em_hasta from editmask within w_info_procesos_historicos
integer x = 1559
integer y = 1660
integer width = 393
integer height = 96
integer taborder = 140
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

type st_8 from statictext within w_info_procesos_historicos
integer x = 347
integer y = 1076
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

type dw_productor from datawindow within w_info_procesos_historicos
integer x = 791
integer y = 1068
integer width = 1138
integer height = 92
integer taborder = 60
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores_clientes"
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

type cbx_productor from checkbox within w_info_procesos_historicos
integer x = 791
integer y = 988
integer width = 402
integer height = 80
integer taborder = 50
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
	istr_mant.argumento[6]	=	'0'
ELSE
	dw_productor.Enabled  = True
	dw_productor.SetFocus()
	dw_productor.Reset()
	dw_productor.InsertRow(0)
END IF
	
end event

type gb_3 from groupbox within w_info_procesos_historicos
integer x = 288
integer y = 1744
integer width = 1760
integer height = 180
integer taborder = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_5 from statictext within w_info_procesos_historicos
integer x = 251
integer y = 1188
integer width = 2203
integer height = 760
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

type cbx_1 from checkbox within w_info_procesos_historicos
integer x = 379
integer y = 1804
integer width = 631
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cajas Equivalentes"
end type

event clicked;IF This.Checked THEN
	tit_peso.Enabled		=	True
	dw_pesoneto.Enabled	=	True
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(255,255,255)))

ELSE
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
END IF

end event

type dw_pesoneto from datawindow within w_info_procesos_historicos
integer x = 1266
integer y = 1804
integer width = 699
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_procesos_historicos
integer x = 1079
integer y = 1804
integer width = 183
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_procesos_historicos
integer x = 347
integer y = 1460
integer width = 279
integer height = 96
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

type st_14 from statictext within w_info_procesos_historicos
integer x = 251
integer y = 684
integer width = 2203
integer height = 504
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

type gb_4 from groupbox within w_info_procesos_historicos
integer x = 297
integer y = 444
integer width = 2094
integer height = 196
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Proceso"
end type

type st_4 from statictext within w_info_procesos_historicos
integer x = 251
integer y = 440
integer width = 2203
integer height = 244
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

type rb_1 from radiobutton within w_info_procesos_historicos
integer x = 407
integer y = 520
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Inspección"
boolean checked = true
end type

event clicked;istr_mant.argumento[7]	=	"1"
	
end event

type rb_2 from radiobutton within w_info_procesos_historicos
integer x = 1120
integer y = 520
integer width = 425
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Condición"
end type

event clicked;istr_mant.argumento[7]	=	"2"
end event

type rb_4 from radiobutton within w_info_procesos_historicos
integer x = 1806
integer y = 520
integer width = 539
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "ReEtiquetado"
end type

event clicked;call super::clicked;istr_mant.argumento[7]	=	"4"
end event

type gb_54 from groupbox within w_info_procesos_historicos
integer x = 297
integer y = 1960
integer width = 2094
integer height = 196
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Informe"
end type

type st_54 from statictext within w_info_procesos_historicos
integer x = 251
integer y = 1948
integer width = 2203
integer height = 244
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

type rb_54 from radiobutton within w_info_procesos_historicos
integer x = 791
integer y = 2036
integer width = 425
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
boolean checked = true
end type

event clicked;istr_mant.argumento[27]	=	"1"
end event

type rb_53 from radiobutton within w_info_procesos_historicos
integer x = 1559
integer y = 2036
integer width = 466
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Tradicional"
end type

event clicked;istr_mant.argumento[27]	=	"2"
end event

type uo_selespecie from uo_seleccion_especie within w_info_procesos_historicos
event destroy ( )
integer x = 791
integer y = 1208
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
		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)

END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_procesos_historicos
event destroy ( )
integer x = 791
integer y = 1392
integer taborder = 260
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_procesos_historicos
integer x = 1737
integer y = 1480
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

