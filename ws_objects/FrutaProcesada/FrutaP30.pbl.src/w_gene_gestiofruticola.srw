$PBExportHeader$w_gene_gestiofruticola.srw
forward
global type w_gene_gestiofruticola from w_para_informes
end type
type rb_despacho from radiobutton within w_gene_gestiofruticola
end type
type rb_recepcion from radiobutton within w_gene_gestiofruticola
end type
type sle_texto from singlelineedit within w_gene_gestiofruticola
end type
type gb_3 from groupbox within w_gene_gestiofruticola
end type
type st_4 from statictext within w_gene_gestiofruticola
end type
type dw_1 from uo_dw within w_gene_gestiofruticola
end type
end forward

global type w_gene_gestiofruticola from w_para_informes
integer x = 14
integer y = 32
integer width = 2565
integer height = 1160
string title = "Termógrafos Por Rango De Fechas"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
rb_despacho rb_despacho
rb_recepcion rb_recepcion
sle_texto sle_texto
gb_3 gb_3
st_4 st_4
dw_1 dw_1
end type
global w_gene_gestiofruticola w_gene_gestiofruticola

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta
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
	FROM	dbo.especies
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
FROM	dbo.plantadesp
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
	FROM	dbo.productores
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

on w_gene_gestiofruticola.create
int iCurrent
call super::create
this.rb_despacho=create rb_despacho
this.rb_recepcion=create rb_recepcion
this.sle_texto=create sle_texto
this.gb_3=create gb_3
this.st_4=create st_4
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_despacho
this.Control[iCurrent+2]=this.rb_recepcion
this.Control[iCurrent+3]=this.sle_texto
this.Control[iCurrent+4]=this.gb_3
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.dw_1
end on

on w_gene_gestiofruticola.destroy
call super::destroy
destroy(this.rb_despacho)
destroy(this.rb_recepcion)
destroy(this.sle_texto)
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.dw_1)
end on

type pb_excel from w_para_informes`pb_excel within w_gene_gestiofruticola
end type

type st_computador from w_para_informes`st_computador within w_gene_gestiofruticola
end type

type st_usuario from w_para_informes`st_usuario within w_gene_gestiofruticola
end type

type st_temporada from w_para_informes`st_temporada within w_gene_gestiofruticola
end type

type p_logo from w_para_informes`p_logo within w_gene_gestiofruticola
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_gene_gestiofruticola
integer width = 1847
string text = "Generacion Informacion Gestion Fruticola"
end type

type pb_acepta from w_para_informes`pb_acepta within w_gene_gestiofruticola
string tag = "Generar Reporte"
integer x = 2153
integer y = 284
integer taborder = 150
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
end type

event pb_acepta::clicked;Long	ll_Total
SetPointer(HourGlass!)

sle_texto.Text = 'Proceso de Carga'

If rb_recepcion.checked Then
	dw_1.DataObject = "dw_gene_gestrecepciones"
	sle_texto.Text +=  ' Recepciones Iniciado'
ElseIf rb_despacho.checked Then
	dw_1.DataObject = "dw_gene_gestdespachos"
	sle_texto.Text += ' Despachos Iniciado'
End If

dw_1.SetTransObject(Sqlca)
ll_Total = dw_1.Retrieve()

If ll_Total = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Total = 0 Then
	MessageBox( "No Existe información", "No se pudo generar información.", StopSign!, Ok!)
Else
	sle_texto.Text = 'Proceso de Carga Terminado con ' + String(dw_1.Object.Filas[1], '#,##0') + ' - Registros Cargados.'
End If

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_gene_gestiofruticola
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2153
integer y = 668
integer taborder = 170
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type rb_despacho from radiobutton within w_gene_gestiofruticola
integer x = 498
integer y = 524
integer width = 402
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
string text = "Despachos"
boolean checked = true
boolean lefttext = true
end type

type rb_recepcion from radiobutton within w_gene_gestiofruticola
integer x = 1307
integer y = 524
integer width = 453
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
string text = "Recepciones"
boolean lefttext = true
end type

type sle_texto from singlelineedit within w_gene_gestiofruticola
integer x = 293
integer y = 676
integer width = 1760
integer height = 160
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
boolean italic = true
long textcolor = 65535
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type gb_3 from groupbox within w_gene_gestiofruticola
integer x = 311
integer y = 448
integer width = 1728
integer height = 192
integer taborder = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Tipo de Carga "
end type

type st_4 from statictext within w_gene_gestiofruticola
integer x = 247
integer y = 412
integer width = 1847
integer height = 500
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

type dw_1 from uo_dw within w_gene_gestiofruticola
boolean visible = false
integer x = 1541
integer y = 36
integer width = 224
integer height = 172
integer taborder = 11
boolean bringtotop = true
boolean vscrollbar = false
end type

