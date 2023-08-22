$PBExportHeader$w_info_mues_anula_inspeccion.srw
$PBExportComments$MantenciónAnulación de Inspección por Vencimiento.
forward
global type w_info_mues_anula_inspeccion from w_para_informes
end type
type st_4 from statictext within w_info_mues_anula_inspeccion
end type
type st_1 from statictext within w_info_mues_anula_inspeccion
end type
type st_5 from statictext within w_info_mues_anula_inspeccion
end type
type dw_cliente from datawindow within w_info_mues_anula_inspeccion
end type
type st_6 from statictext within w_info_mues_anula_inspeccion
end type
type dw_planta from datawindow within w_info_mues_anula_inspeccion
end type
type st_3 from statictext within w_info_mues_anula_inspeccion
end type
type dw_especie from datawindow within w_info_mues_anula_inspeccion
end type
type st_7 from statictext within w_info_mues_anula_inspeccion
end type
type em_variedad from editmask within w_info_mues_anula_inspeccion
end type
type cb_buscavariedad from commandbutton within w_info_mues_anula_inspeccion
end type
type sle_variedad from singlelineedit within w_info_mues_anula_inspeccion
end type
type cbx_variedad from checkbox within w_info_mues_anula_inspeccion
end type
type st_8 from statictext within w_info_mues_anula_inspeccion
end type
type st_9 from statictext within w_info_mues_anula_inspeccion
end type
type em_anula from editmask within w_info_mues_anula_inspeccion
end type
type em_fecha from editmask within w_info_mues_anula_inspeccion
end type
type st_10 from statictext within w_info_mues_anula_inspeccion
end type
type st_11 from statictext within w_info_mues_anula_inspeccion
end type
type em_dias from editmask within w_info_mues_anula_inspeccion
end type
end forward

global type w_info_mues_anula_inspeccion from w_para_informes
integer x = 14
integer y = 32
integer width = 2747
integer height = 1808
string title = "Informe de Anulaciones S.A.G."
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_5 st_5
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
st_9 st_9
em_anula em_anula
em_fecha em_fecha
st_10 st_10
st_11 st_11
em_dias em_dias
end type
global w_info_mues_anula_inspeccion w_info_mues_anula_inspeccion

type variables
Integer ii_existe

str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_especie
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

on w_info_mues_anula_inspeccion.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
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
this.st_9=create st_9
this.em_anula=create em_anula
this.em_fecha=create em_fecha
this.st_10=create st_10
this.st_11=create st_11
this.em_dias=create em_dias
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.dw_cliente
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_planta
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.dw_especie
this.Control[iCurrent+9]=this.st_7
this.Control[iCurrent+10]=this.em_variedad
this.Control[iCurrent+11]=this.cb_buscavariedad
this.Control[iCurrent+12]=this.sle_variedad
this.Control[iCurrent+13]=this.cbx_variedad
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.st_9
this.Control[iCurrent+16]=this.em_anula
this.Control[iCurrent+17]=this.em_fecha
this.Control[iCurrent+18]=this.st_10
this.Control[iCurrent+19]=this.st_11
this.Control[iCurrent+20]=this.em_dias
end on

on w_info_mues_anula_inspeccion.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
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
destroy(this.st_9)
destroy(this.em_anula)
destroy(this.em_fecha)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.em_dias)
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


istr_mant.argumento[4]	=	'0'
istr_mant.argumento[5]	=	ls_Planta
istr_mant.argumento[11]	=	String(li_region)


dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)

em_fecha.Text				=	String(Today())
ii_existe					= 0
pb_acepta.Enabled			= False
//buscar	=	"Número de Pallet:Npaen_numero,Estado Anterior:Ncoca_estant," + &
//				"Estado Nuevo:Ncoca_estneo"
//ordenar	=	"Número de Pallet:paen_numero,Estado Anterior:coca_estant," + &
//				"Estado Nuevo:coca_estneo"
end event

type pb_excel from w_para_informes`pb_excel within w_info_mues_anula_inspeccion
end type

type st_computador from w_para_informes`st_computador within w_info_mues_anula_inspeccion
end type

type st_usuario from w_para_informes`st_usuario within w_info_mues_anula_inspeccion
end type

type st_temporada from w_para_informes`st_temporada within w_info_mues_anula_inspeccion
end type

type p_logo from w_para_informes`p_logo within w_info_mues_anula_inspeccion
end type

type st_titulo from w_para_informes`st_titulo within w_info_mues_anula_inspeccion
integer width = 1957
string text = "Informe para Revisión de Vencimientos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_mues_anula_inspeccion
string tag = "Imprimir Reporte"
integer x = 2386
integer y = 1024
integer taborder = 100
boolean enabled = false
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Long		ll_Fila, ll_Respuesta, ll_Numero
Integer	li_Cliente, li_Planta, li_Especie, li_Variedad, li_Dias
Date		ld_FechaControl

li_Cliente			=	dw_cliente.Object.clie_codigo[1]
li_Planta			=	dw_planta.Object.plde_codigo[1]
ll_Numero			=	Long(em_anula.Text)
////ld_FechaControl	=	RelativeDate(Date(em_Fecha.Text), Integer(em_dias.Text) * -1)
ld_FechaControl	=	Date(em_Fecha.Text)
li_Especie			=	Integer(istr_mant.argumento[3])
li_Variedad			=	Integer(istr_mant.argumento[9])
li_Dias				=	Integer(em_dias.Text)


IF	ii_existe= 0 and li_Dias <= 0 THEN
		MessageBox("Atención", "Faltan Información para el Filtro de Anulación.~r~rComplete la Información.", &
						Exclamation!, Ok!)
		em_anula.SetFocus()

ELSE
		istr_info.titulo	= 'INFORME PARA REVISION DE VENCIMIENTOS'

		OpenWithParm(vinf, istr_info)
		
		vinf.dw_1.DataObject = "dw_info_mues_anula_inspecpaldet_pro"
		
		vinf.dw_1.SetTransObject(sqlca)
		
		ll_fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
												Integer(istr_mant.Argumento[2]), &
												ld_FechaControl, &
												ll_Numero, &
												Long(istr_mant.Argumento[3]), &
												Integer(istr_mant.Argumento[9]), &
												li_Dias)
											 
		IF ll_fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		
		ELSEIF ll_fila = 0 THEN
			MessageBox( "No Existe información", "No existe información para este informe.", &
							 StopSign!, Ok!)
		
		ELSE
			F_Membrete(vinf.dw_1)
			IF ii_existe = 0 THEN			
				vinf.dw_1.Modify("t_dias.text = '" + String(li_Dias) + "'")			
			END IF
			vinf.dw_1.Modify("t_fecha_corte.text = '" + String(ld_FechaControl) + "'")
			vinf.dw_1.Modify("t_nro_anula.text = '"+ String(ll_Numero) + "'")
			IF gs_Ambiente <> 'Windows' THEN
				F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
			END IF
		END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_mues_anula_inspeccion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2382
integer y = 1336
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_mues_anula_inspeccion
integer x = 251
integer y = 440
integer width = 1961
integer height = 340
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_mues_anula_inspeccion
integer x = 306
integer y = 624
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

type st_5 from statictext within w_info_mues_anula_inspeccion
integer x = 251
integer y = 1148
integer width = 1961
integer height = 416
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_mues_anula_inspeccion
integer x = 695
integer y = 492
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

type st_6 from statictext within w_info_mues_anula_inspeccion
integer x = 306
integer y = 500
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

type dw_planta from datawindow within w_info_mues_anula_inspeccion
integer x = 695
integer y = 612
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

type st_3 from statictext within w_info_mues_anula_inspeccion
integer x = 306
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

type dw_especie from datawindow within w_info_mues_anula_inspeccion
integer x = 695
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

type st_7 from statictext within w_info_mues_anula_inspeccion
integer x = 306
integer y = 1008
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

type em_variedad from editmask within w_info_mues_anula_inspeccion
integer x = 695
integer y = 984
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

event modified;Integer		li_especie, li_variedad
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
end event

type cb_buscavariedad from commandbutton within w_info_mues_anula_inspeccion
integer x = 914
integer y = 992
integer width = 96
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
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

type sle_variedad from singlelineedit within w_info_mues_anula_inspeccion
integer x = 1019
integer y = 988
integer width = 841
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type cbx_variedad from checkbox within w_info_mues_anula_inspeccion
integer x = 1870
integer y = 1004
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
ELSE
	em_variedad.Enabled			=	True
	cb_buscavariedad.Enabled	=	True
END IF
end event

type st_8 from statictext within w_info_mues_anula_inspeccion
integer x = 251
integer y = 780
integer width = 1961
integer height = 368
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_mues_anula_inspeccion
integer x = 430
integer y = 1204
integer width = 553
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
string text = "Anulación S.A.G."
boolean focusrectangle = false
end type

type em_anula from editmask within w_info_mues_anula_inspeccion
integer x = 1010
integer y = 1196
integer width = 434
integer height = 92
integer taborder = 70
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
string mask = "#####"
string displaydata = "@"
end type

event modified;Long		ll_Fila, ll_Respuesta, ll_Numero
Integer	li_Cliente, li_Planta
Date		ld_FechaAnula, ld_Fecha

li_Cliente			=	dw_cliente.Object.clie_codigo[1]
li_Planta			=	dw_planta.Object.plde_codigo[1]
ll_Numero			=	Long(em_anula.Text)

ii_existe			= 0
IF This.Text <> "" THEN
	SELECT DISTINCT inpd_fechaa
		INTO	:ld_FechaAnula 
		FROM	dbo.inspecpaldet
		WHERE	clie_Codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta
		AND	inpd_nroanu	=	:ll_Numero ;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Detalle Inspección")
	ELSEIF Not IsNull(ld_FechaAnula) AND ld_FechaAnula <> ld_Fecha THEN
		em_fecha.Text			=	String(ld_FechaAnula)
		em_dias.Text			=  ''
		em_fecha.Enabled		=	False
		em_dias.Enabled		=  False
		ii_existe				= 	1		
		pb_acepta.Enabled		= 	True
		pb_acepta.SetFocus()
		
	ELSE
		em_fecha.Text				=	String(Today())
		em_dias.Text			=  '30'
		
		em_fecha.Enabled		=	True
		em_dias.Enabled		=  True
		pb_acepta.Enabled		= 	True
		em_fecha.SetFocus()
	END IF

END IF
 


end event

type em_fecha from editmask within w_info_mues_anula_inspeccion
integer x = 1010
integer y = 1312
integer width = 434
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

type st_10 from statictext within w_info_mues_anula_inspeccion
integer x = 777
integer y = 1320
integer width = 206
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
string text = "Fecha"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_mues_anula_inspeccion
integer x = 603
integer y = 1436
integer width = 379
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
string text = "Vencimiento"
boolean focusrectangle = false
end type

type em_dias from editmask within w_info_mues_anula_inspeccion
integer x = 1230
integer y = 1424
integer width = 215
integer height = 92
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "30"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "###"
string displaydata = "@"
end type

event modified;Integer li_Dias

li_Dias	= Integer(This.text)

IF	ii_existe= 0 and li_Dias <= 0 THEN
	pb_acepta.Enabled	= False
	This.text	= ""
	This.Setfocus()
ELSE
	pb_acepta.Enabled	= True
END IF
end event

