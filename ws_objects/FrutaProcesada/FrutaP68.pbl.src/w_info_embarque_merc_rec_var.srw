$PBExportHeader$w_info_embarque_merc_rec_var.srw
forward
global type w_info_embarque_merc_rec_var from w_para_informes
end type
type st_4 from statictext within w_info_embarque_merc_rec_var
end type
type st_1 from statictext within w_info_embarque_merc_rec_var
end type
type st_2 from statictext within w_info_embarque_merc_rec_var
end type
type em_desde from editmask within w_info_embarque_merc_rec_var
end type
type dw_cliente from datawindow within w_info_embarque_merc_rec_var
end type
type st_6 from statictext within w_info_embarque_merc_rec_var
end type
type dw_planta from datawindow within w_info_embarque_merc_rec_var
end type
type st_3 from statictext within w_info_embarque_merc_rec_var
end type
type cbx_planta from checkbox within w_info_embarque_merc_rec_var
end type
type st_7 from statictext within w_info_embarque_merc_rec_var
end type
type em_hasta from editmask within w_info_embarque_merc_rec_var
end type
type st_8 from statictext within w_info_embarque_merc_rec_var
end type
type dw_mercado from datawindow within w_info_embarque_merc_rec_var
end type
type cbx_mercado from checkbox within w_info_embarque_merc_rec_var
end type
type st_9 from statictext within w_info_embarque_merc_rec_var
end type
type cbx_recibidor from checkbox within w_info_embarque_merc_rec_var
end type
type dw_recibidor from datawindow within w_info_embarque_merc_rec_var
end type
type gb_3 from groupbox within w_info_embarque_merc_rec_var
end type
type st_5 from statictext within w_info_embarque_merc_rec_var
end type
type cbx_peso from checkbox within w_info_embarque_merc_rec_var
end type
type dw_pesoneto from datawindow within w_info_embarque_merc_rec_var
end type
type tit_peso from statictext within w_info_embarque_merc_rec_var
end type
type st_variedad from statictext within w_info_embarque_merc_rec_var
end type
type cbx_variedad from checkbox within w_info_embarque_merc_rec_var
end type
type em_variedad from editmask within w_info_embarque_merc_rec_var
end type
type cb_buscavariedad from commandbutton within w_info_embarque_merc_rec_var
end type
type sle_variedad from singlelineedit within w_info_embarque_merc_rec_var
end type
type uo_selespecie from uo_seleccion_especie within w_info_embarque_merc_rec_var
end type
type cbx_varirotula from checkbox within w_info_embarque_merc_rec_var
end type
end forward

global type w_info_embarque_merc_rec_var from w_para_informes
integer x = 14
integer y = 32
integer width = 2624
integer height = 2196
string title = "Mercado/Recibidor/Variedad"
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
dw_planta dw_planta
st_3 st_3
cbx_planta cbx_planta
st_7 st_7
em_hasta em_hasta
st_8 st_8
dw_mercado dw_mercado
cbx_mercado cbx_mercado
st_9 st_9
cbx_recibidor cbx_recibidor
dw_recibidor dw_recibidor
gb_3 gb_3
st_5 st_5
cbx_peso cbx_peso
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_variedad st_variedad
cbx_variedad cbx_variedad
em_variedad em_variedad
cb_buscavariedad cb_buscavariedad
sle_variedad sle_variedad
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
end type
global w_info_embarque_merc_rec_var w_info_embarque_merc_rec_var

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor,&
						idwc_packing, idwc_pesoneto, idwc_mercado, idwc_recibidor

String is_NomPlanta
uo_seleccion_especie		iuo_selespecie
end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean existepacking (integer li_planta)
public function boolean existeproductor (long ll_productor)
end prototypes

public function boolean existeespecie (integer especie);String		ls_Nombre

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

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
WHERE	 plde_codigo =  :li_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[7] = String(li_planta)
	RETURN True 
END IF
end function

public function boolean existeproductor (long ll_productor);String	ls_Nombre

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

on w_info_embarque_merc_rec_var.create
int iCurrent
call super::create
this.st_4=create st_4
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
this.dw_mercado=create dw_mercado
this.cbx_mercado=create cbx_mercado
this.st_9=create st_9
this.cbx_recibidor=create cbx_recibidor
this.dw_recibidor=create dw_recibidor
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_peso=create cbx_peso
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.cbx_variedad=create cbx_variedad
this.em_variedad=create em_variedad
this.cb_buscavariedad=create cb_buscavariedad
this.sle_variedad=create sle_variedad
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.cbx_planta
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.em_hasta
this.Control[iCurrent+12]=this.st_8
this.Control[iCurrent+13]=this.dw_mercado
this.Control[iCurrent+14]=this.cbx_mercado
this.Control[iCurrent+15]=this.st_9
this.Control[iCurrent+16]=this.cbx_recibidor
this.Control[iCurrent+17]=this.dw_recibidor
this.Control[iCurrent+18]=this.gb_3
this.Control[iCurrent+19]=this.st_5
this.Control[iCurrent+20]=this.cbx_peso
this.Control[iCurrent+21]=this.dw_pesoneto
this.Control[iCurrent+22]=this.tit_peso
this.Control[iCurrent+23]=this.st_variedad
this.Control[iCurrent+24]=this.cbx_variedad
this.Control[iCurrent+25]=this.em_variedad
this.Control[iCurrent+26]=this.cb_buscavariedad
this.Control[iCurrent+27]=this.sle_variedad
this.Control[iCurrent+28]=this.uo_selespecie
this.Control[iCurrent+29]=this.cbx_varirotula
end on

on w_info_embarque_merc_rec_var.destroy
call super::destroy
destroy(this.st_4)
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
destroy(this.dw_mercado)
destroy(this.cbx_mercado)
destroy(this.st_9)
destroy(this.cbx_recibidor)
destroy(this.dw_recibidor)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_peso)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.cbx_variedad)
destroy(this.em_variedad)
destroy(this.cb_buscavariedad)
destroy(this.sle_variedad)
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
//dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_recibidor.GetChild("reci_codigo", idwc_recibidor)
idwc_recibidor.SetTransObject(sqlca)
idwc_recibidor.Retrieve()
dw_recibidor.InsertRow(0)

dw_mercado.GetChild("merc_codigo", idwc_mercado)
idwc_mercado.SetTransObject(sqlca)
idwc_mercado.Retrieve()
dw_mercado.InsertRow(0)

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve()
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 8.20)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	"0" //String(gi_codplanta)		//	planta
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
//istr_mant.argumento[5]	=	String(gi_CodEspecie)	//	especie
istr_mant.argumento[6]  =  "0"								//	mercado
istr_mant.argumento[7]  =  "0"								//	recibidor
istr_mant.argumento[8]  =  "1"							//	peso

end event

type st_computador from w_para_informes`st_computador within w_info_embarque_merc_rec_var
end type

type st_usuario from w_para_informes`st_usuario within w_info_embarque_merc_rec_var
end type

type st_temporada from w_para_informes`st_temporada within w_info_embarque_merc_rec_var
end type

type p_logo from w_para_informes`p_logo within w_info_embarque_merc_rec_var
end type

type st_titulo from w_para_informes`st_titulo within w_info_embarque_merc_rec_var
integer width = 1847
string text = "Informe Embarques Mercado/Recibidor/Variedad"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_embarque_merc_rec_var
string tag = "Imprimir Reporte"
integer x = 2245
integer y = 1452
integer taborder = 150
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_varirotula
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_cajas, ls_especie, ls_planta, &
			ls_mercado, ls_recibidor

istr_info.titulo	= 'INFORME DE EMBARQUES MERCADO/RECIBIDOR/VARIEDAD'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_embarque_merc_rec_var"

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
ld_desde			=	Date(istr_mant.argumento[3])
ld_hasta			=	Date(istr_mant.argumento[4])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF cbx_peso.Checked=False THEN
	ls_cajas = "Bulto"
	istr_mant.argumento[8]	=	"1"
ELSE
	istr_mant.argumento[8]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas						=	"Base " + istr_mant.argumento[8] 
END IF

IF cbx_planta.Checked THEN
	ls_planta	=	'Todas'
ELSE
	ls_planta	=	idwc_planta.GetItemString(idwc_planta.GetRow(),"plde_nombre")
END IF
IF cbx_mercado.Checked THEN
	ls_mercado	=	'Todos'
ELSE
	ls_mercado	=	idwc_mercado.GetItemString(idwc_mercado.GetRow(),"merc_nombre")
END IF
IF cbx_recibidor.Checked THEN
	ls_recibidor = 'Todos'
ELSE
	ls_recibidor = idwc_recibidor.GetItemString(idwc_recibidor.GetRow(),"reci_nombre")
END IF

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, uo_selespecie.Codigo, &
					Dec(istr_mant.argumento[8]),Long(istr_mant.argumento[7]), &
					Integer(istr_mant.argumento[6]), ld_desde, ld_hasta, li_varirotula)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("especie.text = '" + uo_selespecie.Nombre + "'")
		vinf.dw_1.Modify("planta.text = '" + ls_planta + "'")
		vinf.dw_1.Modify("mercado.text = '" + ls_mercado + "'")
		vinf.dw_1.Modify("recibidor.text = '" + ls_recibidor + "'")
		vinf.dw_1.Modify("Base.text = '" + ls_cajas + "'")
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_embarque_merc_rec_var
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2245
integer y = 1740
integer taborder = 170
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_embarque_merc_rec_var
integer x = 251
integer y = 440
integer width = 1847
integer height = 812
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

type st_1 from statictext within w_info_embarque_merc_rec_var
integer x = 343
integer y = 700
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

type st_2 from statictext within w_info_embarque_merc_rec_var
integer x = 347
integer y = 1500
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

type em_desde from editmask within w_info_embarque_merc_rec_var
integer x = 791
integer y = 1484
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

type dw_cliente from datawindow within w_info_embarque_merc_rec_var
integer x = 786
integer y = 516
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
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_embarque_merc_rec_var
integer x = 343
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
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_embarque_merc_rec_var
integer x = 786
integer y = 704
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

type st_3 from statictext within w_info_embarque_merc_rec_var
integer x = 347
integer y = 1368
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

type cbx_planta from checkbox within w_info_embarque_merc_rec_var
integer x = 786
integer y = 628
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

type st_7 from statictext within w_info_embarque_merc_rec_var
integer x = 1225
integer y = 1500
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

type em_hasta from editmask within w_info_embarque_merc_rec_var
integer x = 1559
integer y = 1484
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

type st_8 from statictext within w_info_embarque_merc_rec_var
integer x = 343
integer y = 916
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
string text = "Mercado"
boolean focusrectangle = false
end type

type dw_mercado from datawindow within w_info_embarque_merc_rec_var
integer x = 786
integer y = 908
integer width = 987
integer height = 92
integer taborder = 60
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_mercado"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;RETURN 1
end event

event itemchanged;istr_mant.argumento[6]	=	data	

end event

type cbx_mercado from checkbox within w_info_embarque_merc_rec_var
integer x = 786
integer y = 828
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

event clicked;IF cbx_mercado.Checked = TRUE THEN
	dw_mercado.Enabled	= False
	dw_mercado.Reset()
	dw_mercado.insertrow(0)
	istr_mant.argumento[6]	=	'0'
ELSE
	dw_mercado.Enabled  = True
	dw_mercado.SetFocus()
	dw_mercado.Reset()
	dw_mercado.InsertRow(0)
END IF
	
end event

type st_9 from statictext within w_info_embarque_merc_rec_var
integer x = 343
integer y = 1104
integer width = 416
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
string text = "Consignatario"
alignment alignment = right!
boolean focusrectangle = false
end type

type cbx_recibidor from checkbox within w_info_embarque_merc_rec_var
integer x = 786
integer y = 1024
integer width = 402
integer height = 80
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked = True THEN
	dw_recibidor.Enabled = False
	istr_mant.argumento[7]	=	'0'
ELSE
	dw_recibidor.Enabled=True	
END IF	
end event

type dw_recibidor from datawindow within w_info_embarque_merc_rec_var
integer x = 786
integer y = 1104
integer width = 1115
integer height = 100
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_consignatarios"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[7]	=	data		

end event

type gb_3 from groupbox within w_info_embarque_merc_rec_var
integer x = 293
integer y = 1588
integer width = 1723
integer height = 280
integer taborder = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_5 from statictext within w_info_embarque_merc_rec_var
integer x = 251
integer y = 1256
integer width = 1847
integer height = 752
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

type cbx_peso from checkbox within w_info_embarque_merc_rec_var
integer x = 352
integer y = 1704
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

type dw_pesoneto from datawindow within w_info_embarque_merc_rec_var
integer x = 1266
integer y = 1704
integer width = 704
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_embarque_merc_rec_var
integer x = 1074
integer y = 1704
integer width = 183
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_embarque_merc_rec_var
boolean visible = false
integer x = 347
integer y = 1484
integer width = 279
integer height = 96
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

type cbx_variedad from checkbox within w_info_embarque_merc_rec_var
boolean visible = false
integer x = 791
integer y = 1400
integer width = 402
integer height = 80
integer taborder = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
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

type em_variedad from editmask within w_info_embarque_merc_rec_var
boolean visible = false
integer x = 791
integer y = 1484
integer width = 215
integer height = 96
integer taborder = 110
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

event modified;Integer		li_cliente,	li_especie, li_variedad
String		ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
li_especie	=	Integer(istr_mant.argumento[5]) // Especie
li_variedad	=	Integer(This.Text)

SELECT	vari_nombre
	INTO	:ls_Nombre
	FROM	"dba"."variedades"
	WHERE	clie_codigo	=	:li_cliente
	AND	espe_codigo	=	:li_especie
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
	istr_mant.argumento[9]	=	String(li_variedad)	
END IF
end event

type cb_buscavariedad from commandbutton within w_info_embarque_merc_rec_var
boolean visible = false
integer x = 1015
integer y = 1492
integer width = 96
integer height = 84
integer taborder = 120
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

type sle_variedad from singlelineedit within w_info_embarque_merc_rec_var
boolean visible = false
integer x = 1115
integer y = 1472
integer width = 841
integer height = 96
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
borderstyle borderstyle = stylelowered!
end type

type uo_selespecie from uo_seleccion_especie within w_info_embarque_merc_rec_var
event destroy ( )
integer x = 786
integer y = 1288
integer height = 160
integer taborder = 100
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cbx_varirotula from checkbox within w_info_embarque_merc_rec_var
integer x = 800
integer y = 1888
integer width = 745
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

