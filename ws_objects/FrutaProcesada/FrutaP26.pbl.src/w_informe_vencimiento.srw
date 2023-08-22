$PBExportHeader$w_informe_vencimiento.srw
$PBExportComments$MantenciónAnulación de Inspección por Vencimiento.
forward
global type w_informe_vencimiento from w_para_informes
end type
type st_4 from statictext within w_informe_vencimiento
end type
type st_1 from statictext within w_informe_vencimiento
end type
type dw_cliente from datawindow within w_informe_vencimiento
end type
type st_6 from statictext within w_informe_vencimiento
end type
type dw_planta from datawindow within w_informe_vencimiento
end type
type st_3 from statictext within w_informe_vencimiento
end type
type dw_especie from datawindow within w_informe_vencimiento
end type
type st_7 from statictext within w_informe_vencimiento
end type
type em_variedad from editmask within w_informe_vencimiento
end type
type cb_buscavariedad from commandbutton within w_informe_vencimiento
end type
type sle_variedad from singlelineedit within w_informe_vencimiento
end type
type cbx_variedad from checkbox within w_informe_vencimiento
end type
type st_8 from statictext within w_informe_vencimiento
end type
type em_fecha from editmask within w_informe_vencimiento
end type
type st_10 from statictext within w_informe_vencimiento
end type
type cbx_seedles from checkbox within w_informe_vencimiento
end type
end forward

global type w_informe_vencimiento from w_para_informes
integer x = 14
integer y = 32
integer width = 2866
integer height = 1812
string title = "Informe de Vencimientos"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
dw_especie dw_especie
st_7 st_7
em_variedad em_variedad
cb_buscavariedad cb_buscavariedad
sle_variedad sle_variedad
cbx_variedad cbx_variedad
st_8 st_8
em_fecha em_fecha
st_10 st_10
cbx_seedles cbx_seedles
end type
global w_informe_vencimiento w_informe_vencimiento

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente, idwc_planta,idwc_especie
end variables

forward prototypes
public function boolean existeespecie (integer especie)
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

on w_informe_vencimiento.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.dw_especie=create dw_especie
this.st_7=create st_7
this.em_variedad=create em_variedad
this.cb_buscavariedad=create cb_buscavariedad
this.sle_variedad=create sle_variedad
this.cbx_variedad=create cbx_variedad
this.st_8=create st_8
this.em_fecha=create em_fecha
this.st_10=create st_10
this.cbx_seedles=create cbx_seedles
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_cliente
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.dw_especie
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.em_variedad
this.Control[iCurrent+10]=this.cb_buscavariedad
this.Control[iCurrent+11]=this.sle_variedad
this.Control[iCurrent+12]=this.cbx_variedad
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.em_fecha
this.Control[iCurrent+15]=this.st_10
this.Control[iCurrent+16]=this.cbx_seedles
end on

on w_informe_vencimiento.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.dw_especie)
destroy(this.st_7)
destroy(this.em_variedad)
destroy(this.cb_buscavariedad)
destroy(this.sle_variedad)
destroy(this.cbx_variedad)
destroy(this.st_8)
destroy(this.em_fecha)
destroy(this.st_10)
destroy(this.cbx_seedles)
end on

event open;call super::open;String	ls_Planta
Integer	li_region

//SELECT	plde_nombre, plde_region
//	INTO	:ls_Planta,:li_region
//	FROM	dba.plantadesp
//	WHERE	plde_codigo	=	:gi_CodPlanta ;

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

istr_mant.argumento[1]		= 	String(gi_codexport)		//	Cliente
istr_mant.argumento[2]		= 	String(gi_CodPlanta)		//	Planta 
istr_mant.argumento[3]		= 	String(gi_codespecie)	//	Especie
istr_mant.argumento[9]		= 	"-1" 							// variedad
istr_mant.argumento[10]    =  "0"                     // Seedless


istr_mant.argumento[5]	=	ls_Planta                  //nombre planta
istr_mant.argumento[11]	=	String(li_region)


dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)

em_fecha.Text				=	String(Today())
//pb_acepta.Enabled			= False
end event

type pb_excel from w_para_informes`pb_excel within w_informe_vencimiento
end type

type st_computador from w_para_informes`st_computador within w_informe_vencimiento
end type

type st_usuario from w_para_informes`st_usuario within w_informe_vencimiento
end type

type st_temporada from w_para_informes`st_temporada within w_informe_vencimiento
end type

type p_logo from w_para_informes`p_logo within w_informe_vencimiento
end type

type st_titulo from w_para_informes`st_titulo within w_informe_vencimiento
integer width = 2075
string text = "Informe de Vencimientos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_informe_vencimiento
string tag = "Imprimir Reporte"
integer x = 2519
integer y = 920
integer taborder = 100
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Long		ll_Fila
Integer	li_Cliente, li_Planta, li_Especie, li_Variedad, li_seedless
Date		ld_FechaControl

li_Cliente			=	dw_cliente.Object.clie_codigo[1]
li_Planta			=	dw_planta.Object.plde_codigo[1]
ld_FechaControl	=	Date(em_Fecha.Text)
li_Especie			=	Integer(istr_mant.argumento[3])
li_Variedad			=	Integer(istr_mant.argumento[9])
li_seedless       =  Integer(istr_mant.argumento[10])


istr_info.titulo	= 'INFORME DE INSPECCIONES A VENCER'

OpenWithParm(vinf, istr_info)
		
vinf.dw_1.DataObject = "dw_informevencimientos"
		
vinf.dw_1.SetTransObject(sqlca)
		
ll_fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),Integer(istr_mant.Argumento[2]),&
										 Integer(istr_mant.Argumento[3]),Integer(istr_mant.Argumento[9]),&
										 Integer(istr_mant.Argumento[10]),ld_FechaControl)
											 
IF ll_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		
ELSEIF ll_fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						 StopSign!, Ok!)
		
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("t_fecha.text = '" + String(ld_FechaControl, 'dd/mm/yyyy') + "'") 
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
		
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_informe_vencimiento
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2510
integer y = 1232
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_informe_vencimiento
integer x = 247
integer y = 440
integer width = 2075
integer height = 340
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

type st_1 from statictext within w_informe_vencimiento
integer x = 320
integer y = 620
integer width = 462
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

type dw_cliente from datawindow within w_informe_vencimiento
integer x = 736
integer y = 488
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	
	idwc_planta.Retrieve(1)
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_informe_vencimiento
integer x = 320
integer y = 496
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

type dw_planta from datawindow within w_informe_vencimiento
integer x = 736
integer y = 608
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	dw_cliente.Object.clie_codigo[1]

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
	istr_mant.Argumento[5]	=	ls_Columna[1]
	istr_mant.Argumento[11]	=	ls_Columna[11]
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_informe_vencimiento
integer x = 320
integer y = 844
integer width = 274
integer height = 84
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
string text = "Especie"
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_informe_vencimiento
integer x = 736
integer y = 828
integer width = 878
integer height = 100
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExisteEspecie(Integer(data)) THEN
	istr_mant.argumento[3]	=	data
ELSE
	This.SetItem(1, "espe_codigo", gi_CodEspecie)
	istr_mant.argumento[3]	=	String(gi_CodEspecie)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_7 from statictext within w_informe_vencimiento
integer x = 320
integer y = 1072
integer width = 302
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
string text = "Variedad"
boolean focusrectangle = false
end type

type em_variedad from editmask within w_informe_vencimiento
integer x = 736
integer y = 1048
integer width = 215
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "####"
end type

event modified;Integer		li_especie, li_variedad,li_seedle
String		ls_Nombre

li_especie	=	Integer(istr_mant.argumento[3]) // Especie
li_variedad	=	Integer(This.Text)

SELECT	vari_nombre
	INTO	:ls_Nombre
	FROM	dbo.variedades
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
	istr_mant.argumento[9]	=	String(li_variedad)	
END IF


 SELECT vari_seedle
    INTO :li_seedle
    FROM dbo.variedades
	 WHERE vari_codigo = :li_variedad;
	 
	IF IsNull(li_seedle) OR li_seedle = 0 THEN
		cbx_seedles.Checked = FALSE
		cbx_seedles.Enabled = FALSE
		istr_mant.argumento[10] = '0'
	ELSE
		cbx_seedles.Checked = TRUE
		cbx_seedles.Enabled = FALSE
		istr_mant.argumento[10] = '1'
	END IF
	 
end event

type cb_buscavariedad from commandbutton within w_informe_vencimiento
integer x = 955
integer y = 1056
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

lstr_busq.argum[1]	=	istr_mant.argumento[1]  // Cliente
lstr_busq.argum[2]	=	istr_mant.argumento[3]  // Especie

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

type sle_variedad from singlelineedit within w_informe_vencimiento
integer x = 1061
integer y = 1048
integer width = 841
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
borderstyle borderstyle = stylelowered!
end type

type cbx_variedad from checkbox within w_informe_vencimiento
integer x = 727
integer y = 948
integer width = 279
integer height = 80
integer taborder = 60
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
	em_variedad.Enabled			=	False
	cb_buscavariedad.Enabled	=	False
	em_variedad.Text				=	''
	sle_variedad.Text				=	''
	istr_mant.argumento[9]		=	'-1'
	cbx_seedles.Checked        = FALSE
	cbx_seedles.Enabled        = TRUE
ELSE
	em_variedad.Enabled			=	True
	em_variedad.SetFocus()
	cb_buscavariedad.Enabled	=	True
END IF
end event

type st_8 from statictext within w_informe_vencimiento
integer x = 247
integer y = 780
integer width = 2075
integer height = 756
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

type em_fecha from editmask within w_informe_vencimiento
integer x = 736
integer y = 1320
integer width = 448
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

event modified;IF Not f_validafechatempo(date(this.Text)) THEN
	This.Text = ''
	This.SetFocus()
END IF
		
end event

type st_10 from statictext within w_informe_vencimiento
integer x = 320
integer y = 1332
integer width = 357
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
string text = "Fecha Tope"
boolean focusrectangle = false
end type

type cbx_seedles from checkbox within w_informe_vencimiento
integer x = 741
integer y = 1160
integer width = 626
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
string text = "Solo con Semillas "
boolean righttoleft = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[10] = '1'
ELSE
	istr_mant.argumento[10] = '0'
END IF
end event

