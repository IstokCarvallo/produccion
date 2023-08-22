$PBExportHeader$w_info_prodfrigo.srw
forward
global type w_info_prodfrigo from w_para_informes
end type
type gb_4 from groupbox within w_info_prodfrigo
end type
type st_2 from statictext within w_info_prodfrigo
end type
type em_desde from editmask within w_info_prodfrigo
end type
type dw_cliente from datawindow within w_info_prodfrigo
end type
type st_6 from statictext within w_info_prodfrigo
end type
type st_3 from statictext within w_info_prodfrigo
end type
type st_7 from statictext within w_info_prodfrigo
end type
type em_hasta from editmask within w_info_prodfrigo
end type
type gb_3 from groupbox within w_info_prodfrigo
end type
type st_5 from statictext within w_info_prodfrigo
end type
type cbx_peso from checkbox within w_info_prodfrigo
end type
type dw_pesoneto from datawindow within w_info_prodfrigo
end type
type tit_peso from statictext within w_info_prodfrigo
end type
type st_variedad from statictext within w_info_prodfrigo
end type
type rb_variemba from radiobutton within w_info_prodfrigo
end type
type rb_varicali from radiobutton within w_info_prodfrigo
end type
type st_4 from statictext within w_info_prodfrigo
end type
type st_1 from statictext within w_info_prodfrigo
end type
type cbx_status from checkbox within w_info_prodfrigo
end type
type uo_selespecie from uo_seleccion_especie within w_info_prodfrigo
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_prodfrigo
end type
type cbx_varirotula from checkbox within w_info_prodfrigo
end type
type st_8 from statictext within w_info_prodfrigo
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_prodfrigo
end type
end forward

global type w_info_prodfrigo from w_para_informes
integer x = 14
integer y = 32
integer width = 2583
integer height = 2168
string title = "INFORME PRODUCCION HISTORICA"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_4 gb_4
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
st_3 st_3
st_7 st_7
em_hasta em_hasta
gb_3 gb_3
st_5 st_5
cbx_peso cbx_peso
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_variedad st_variedad
rb_variemba rb_variemba
rb_varicali rb_varicali
st_4 st_4
st_1 st_1
cbx_status cbx_status
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
st_8 st_8
uo_selproductor uo_selproductor
end type
global w_info_prodfrigo w_info_prodfrigo

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor, idwc_packing, idwc_pesoneto

String is_NomPlanta

uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad
uo_seleccion_varios_productores	iuo_selproductor
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
	istr_mant.argumento[5] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_prodfrigo.create
int iCurrent
call super::create
this.gb_4=create gb_4
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_3=create st_3
this.st_7=create st_7
this.em_hasta=create em_hasta
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_peso=create cbx_peso
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.rb_variemba=create rb_variemba
this.rb_varicali=create rb_varicali
this.st_4=create st_4
this.st_1=create st_1
this.cbx_status=create cbx_status
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.st_8=create st_8
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_4
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.dw_cliente
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_7
this.Control[iCurrent+8]=this.em_hasta
this.Control[iCurrent+9]=this.gb_3
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.cbx_peso
this.Control[iCurrent+12]=this.dw_pesoneto
this.Control[iCurrent+13]=this.tit_peso
this.Control[iCurrent+14]=this.st_variedad
this.Control[iCurrent+15]=this.rb_variemba
this.Control[iCurrent+16]=this.rb_varicali
this.Control[iCurrent+17]=this.st_4
this.Control[iCurrent+18]=this.st_1
this.Control[iCurrent+19]=this.cbx_status
this.Control[iCurrent+20]=this.uo_selespecie
this.Control[iCurrent+21]=this.uo_selvariedad
this.Control[iCurrent+22]=this.cbx_varirotula
this.Control[iCurrent+23]=this.st_8
this.Control[iCurrent+24]=this.uo_selproductor
end on

on w_info_prodfrigo.destroy
call super::destroy
destroy(this.gb_4)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_peso)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.rb_variemba)
destroy(this.rb_varicali)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.cbx_status)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.st_8)
destroy(this.uo_selproductor)
end on

event open;Boolean lb_Cerrar

x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve(gi_CodExport)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

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

// uo_seleccion_productor
IF IsNull(uo_selproductor.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	//Close(This)
	lb_Cerrar = False
ELSE
	uo_selproductor.Seleccion(True,False)
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
istr_mant.argumento[2]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[3]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[4]	=	String(gi_CodEspecie)	//	especie
istr_mant.argumento[5]  =  "0"							//	productor
istr_mant.argumento[6]  =  "1"							//	peso
istr_mant.argumento[7]	= 	"0" 							// variedad
istr_mant.argumento[8]   =	'0'							// status
istr_mant.argumento[9]	 = "Todos "
end event

type st_computador from w_para_informes`st_computador within w_info_prodfrigo
end type

type st_usuario from w_para_informes`st_usuario within w_info_prodfrigo
end type

type st_temporada from w_para_informes`st_temporada within w_info_prodfrigo
end type

type p_logo from w_para_informes`p_logo within w_info_prodfrigo
end type

type st_titulo from w_para_informes`st_titulo within w_info_prodfrigo
integer width = 1847
string text = "Producción Histórica Frigoríficos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_prodfrigo
integer x = 2240
integer y = 1448
integer taborder = 120
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_nroguia, ls_cajas, ls_descri, ls_lista, ls_produ
Long		ll_produ
Integer	li_varirotula

istr_info.titulo	= 'PRODUCCION HISTORICA FRIGORIFICOS'
OpenWithParm(vinf, istr_info)

IF rb_variemba.Checked THEN
	vinf.dw_1.DataObject = "dw_info_produccion_frigorifico_enc"
ELSE
	vinf.dw_1.DataObject = "dw_info_produccion_frigo_varicali"
END IF	

/*
productor
*/
ls_lista = uo_selproductor.Lista

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

li_cliente 		=	Integer(istr_mant.argumento[1])
ld_desde			=	Date(istr_mant.argumento[2])
ld_hasta			=	Date(istr_mant.argumento[3])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF cbx_peso.Checked=False THEN
	ls_cajas = "Reales"
	istr_mant.argumento[6]	=	"1"
ELSE
	istr_mant.argumento[6]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas						=	"Base " + istr_mant.argumento[6] 
END IF

IF ls_lista 	= '-1' THEN
	ls_descri 	= 'Todos'
	  
ELSE
	IF len(ls_lista) > 5 THEN
		ls_descri 	= ls_lista
		ls_produ 	= ls_lista
	ELSE
		ls_descri 	= uo_selproductor.Nombre
		ls_produ 	= ls_lista
	END IF
	
	
END IF	

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, ld_desde, ld_hasta, uo_selespecie.Codigo,&
									Dec(istr_mant.argumento[6]),uo_selvariedad.Codigo,&
									integer(istr_mant.argumento[8]),li_varirotula,ls_lista)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
//		vinf.dw_1.Object.titulo_informe.text = 'Producción Historica Frigoríficos'
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		vinf.dw_1.Modify("nombre.text = '" + ls_descri+ " (" + ls_produ+")" + "'")
		vinf.dw_1.Modify("Cajas.text = '" + ls_cajas + "'")
		vinf.dw_1.Modify("status.text = '" + istr_mant.argumento[9] + "'")		
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_prodfrigo
integer x = 2240
integer y = 1732
integer taborder = 140
end type

type gb_4 from groupbox within w_info_prodfrigo
integer x = 293
integer y = 1224
integer width = 1742
integer height = 212
integer taborder = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Ordenado por "
end type

type st_2 from statictext within w_info_prodfrigo
integer x = 343
integer y = 1640
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

type em_desde from editmask within w_info_prodfrigo
integer x = 786
integer y = 1624
integer width = 393
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
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[2]	=	This.Text
end event

type dw_cliente from datawindow within w_info_prodfrigo
integer x = 786
integer y = 464
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
	idwc_productor.Retrieve()	
	
	uo_selproductor.Filtra(-1,-1,Integer(data))
	
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_prodfrigo
integer x = 343
integer y = 472
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

type st_3 from statictext within w_info_prodfrigo
integer x = 343
integer y = 944
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

type st_7 from statictext within w_info_prodfrigo
integer x = 1221
integer y = 1640
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

type em_hasta from editmask within w_info_prodfrigo
integer x = 1554
integer y = 1624
integer width = 393
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
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[3]	=	This.Text
end event

type gb_3 from groupbox within w_info_prodfrigo
integer x = 293
integer y = 1720
integer width = 1742
integer height = 212
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_5 from statictext within w_info_prodfrigo
integer x = 247
integer y = 1576
integer width = 1847
integer height = 404
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

type cbx_peso from checkbox within w_info_prodfrigo
integer x = 338
integer y = 1800
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

type dw_pesoneto from datawindow within w_info_prodfrigo
integer x = 1303
integer y = 1796
integer width = 686
integer height = 100
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_prodfrigo
integer x = 1111
integer y = 1808
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

type st_variedad from statictext within w_info_prodfrigo
integer x = 343
integer y = 1136
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

type rb_variemba from radiobutton within w_info_prodfrigo
integer x = 448
integer y = 1312
integer width = 503
integer height = 80
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Embalaje   "
boolean checked = true
boolean lefttext = true
end type

type rb_varicali from radiobutton within w_info_prodfrigo
integer x = 1422
integer y = 1312
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
string text = "Variedad"
boolean lefttext = true
end type

type st_4 from statictext within w_info_prodfrigo
integer x = 247
integer y = 440
integer width = 1847
integer height = 1136
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

type st_1 from statictext within w_info_prodfrigo
integer x = 343
integer y = 1460
integer width = 265
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Status"
boolean focusrectangle = false
end type

type cbx_status from checkbox within w_info_prodfrigo
integer x = 786
integer y = 1460
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
   istr_mant.argumento[8]   =	'1'							// status
   istr_mant.argumento[9]	=  "Consolidados "			// status nombre
ELSE
   istr_mant.argumento[8]   =	'0'							// status
   istr_mant.argumento[9]	=  "Todos "						// status nombre
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

type uo_selespecie from uo_seleccion_especie within w_info_prodfrigo
event destroy ( )
integer x = 786
integer y = 856
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

END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_prodfrigo
event destroy ( )
integer x = 786
integer y = 1040
integer height = 172
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_prodfrigo
integer x = 1719
integer y = 1136
integer width = 334
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
string text = "Var. Rot."
end type

type st_8 from statictext within w_info_prodfrigo
integer x = 343
integer y = 716
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
string text = "Productor"
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_prodfrigo
integer x = 791
integer y = 572
integer taborder = 130
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

