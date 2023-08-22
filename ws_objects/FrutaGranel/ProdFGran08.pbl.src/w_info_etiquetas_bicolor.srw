$PBExportHeader$w_info_etiquetas_bicolor.srw
forward
global type w_info_etiquetas_bicolor from w_para_informes
end type
type tit_peso from statictext within w_info_etiquetas_bicolor
end type
type gb_3 from groupbox within w_info_etiquetas_bicolor
end type
type st_4 from statictext within w_info_etiquetas_bicolor
end type
type st_1 from statictext within w_info_etiquetas_bicolor
end type
type em_cantidad from editmask within w_info_etiquetas_bicolor
end type
type dw_1 from datawindow within w_info_etiquetas_bicolor
end type
end forward

global type w_info_etiquetas_bicolor from w_para_informes
integer x = 14
integer y = 32
integer width = 1879
integer height = 712
string title = "IMPRESION DE ETIQUETAS BICOLOR"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
tit_peso tit_peso
gb_3 gb_3
st_4 st_4
st_1 st_1
em_cantidad em_cantidad
dw_1 dw_1
end type
global w_info_etiquetas_bicolor w_info_etiquetas_bicolor

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie, idwc_camara, idwc_packing, idwc_pesoneto

String is_NomPlanta
end variables

forward prototypes
public function string get_clie_nombre (integer clie_codigo)
public function string get_plde_nombre (integer plde_codigo)
public function boolean existeespecie (integer especie)
public function boolean existepacking (ref string ls_columna)
public function boolean existeproductor (long ll_productor)
end prototypes

public function string get_clie_nombre (integer clie_codigo);string ls_nomcli

SELECT clie_nombre 	
	INTO :ls_nomcli FROM 
	dba.clientesprod WHERE 
	clie_codigo= :clie_codigo;

return ls_nomcli
end function

public function string get_plde_nombre (integer plde_codigo);string ls_nomplde

SELECT plde_nombre 	
	INTO :ls_nomplde FROM 
	dba.plantadesp WHERE 
	plde_codigo= :plde_codigo;

return ls_nomplde

end function

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

public function boolean existepacking (ref string ls_columna);Integer  li_codigo

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

on w_info_etiquetas_bicolor.create
int iCurrent
call super::create
this.tit_peso=create tit_peso
this.gb_3=create gb_3
this.st_4=create st_4
this.st_1=create st_1
this.em_cantidad=create em_cantidad
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tit_peso
this.Control[iCurrent+2]=this.gb_3
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.em_cantidad
this.Control[iCurrent+6]=this.dw_1
end on

on w_info_etiquetas_bicolor.destroy
call super::destroy
destroy(this.tit_peso)
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.em_cantidad)
destroy(this.dw_1)
end on

event resize;//
end event

type st_titulo from w_para_informes`st_titulo within w_info_etiquetas_bicolor
integer x = 82
integer y = 52
integer width = 1417
string text = "Impresión de Etiquetas Bicolor"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_etiquetas_bicolor
string tag = "Imprimir Reporte"
integer x = 1600
integer y = 140
integer taborder = 100
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;Long	fila, ll_cantidad, ll_actual
String	ls_embalaje, ls_calibre, ls_data_object


ls_data_object = "dw_info_spro_cajasprod_fto2"

SetPointer(HourGlass!)
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = ls_data_object

dw_1.SetTransObject(sqlca)
vinf.dw_1.SetTransObject(sqlca)

dw_1.ShareData(vinf.dw_1)

ll_cantidad	=	Long(em_cantidad.Text)

dw_1.Reset()
FOR fila = 1 TO (ll_cantidad / 2)
	ll_actual	=	dw_1.InsertRow(0)
	dw_1.Object.codigo[ll_actual] =	'8 3509800522 0'
NEXT

vinf.Visible	= True
vinf.Enabled	= True

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_etiquetas_bicolor
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1591
integer y = 400
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type tit_peso from statictext within w_info_etiquetas_bicolor
boolean visible = false
integer x = 567
integer y = 1744
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

type gb_3 from groupbox within w_info_etiquetas_bicolor
boolean visible = false
integer x = 174
integer y = 1412
integer width = 1614
integer height = 280
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_4 from statictext within w_info_etiquetas_bicolor
integer x = 78
integer y = 156
integer width = 1426
integer height = 416
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_etiquetas_bicolor
integer x = 160
integer y = 328
integer width = 649
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cantidad de Etiquetas"
alignment alignment = center!
boolean focusrectangle = false
end type

type em_cantidad from editmask within w_info_etiquetas_bicolor
integer x = 859
integer y = 320
integer width = 402
integer height = 88
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#####0"
boolean spin = true
double increment = 1
string minmax = "0~~999999"
end type

type dw_1 from datawindow within w_info_etiquetas_bicolor
boolean visible = false
integer x = 434
integer y = 668
integer width = 686
integer height = 400
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_fto2"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

