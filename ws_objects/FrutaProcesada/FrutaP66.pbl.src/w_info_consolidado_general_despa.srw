$PBExportHeader$w_info_consolidado_general_despa.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_consolidado_general_despa from w_para_informes
end type
type st_1 from statictext within w_info_consolidado_general_despa
end type
type dw_cliente from datawindow within w_info_consolidado_general_despa
end type
type st_2 from statictext within w_info_consolidado_general_despa
end type
type st_3 from statictext within w_info_consolidado_general_despa
end type
type st_8 from statictext within w_info_consolidado_general_despa
end type
type cbx_planta from checkbox within w_info_consolidado_general_despa
end type
type dw_planta from datawindow within w_info_consolidado_general_despa
end type
type st_9 from statictext within w_info_consolidado_general_despa
end type
type st_10 from statictext within w_info_consolidado_general_despa
end type
type st_11 from statictext within w_info_consolidado_general_despa
end type
type dw_tiposalida from datawindow within w_info_consolidado_general_despa
end type
type st_plantadestino from statictext within w_info_consolidado_general_despa
end type
type dw_plantadestino from datawindow within w_info_consolidado_general_despa
end type
type st_22 from statictext within w_info_consolidado_general_despa
end type
type em_desde from editmask within w_info_consolidado_general_despa
end type
type st_23 from statictext within w_info_consolidado_general_despa
end type
type em_hasta from editmask within w_info_consolidado_general_despa
end type
type st_21 from statictext within w_info_consolidado_general_despa
end type
type st_5 from statictext within w_info_consolidado_general_despa
end type
type dw_transp from datawindow within w_info_consolidado_general_despa
end type
type cbx_trans from checkbox within w_info_consolidado_general_despa
end type
type st_4 from statictext within w_info_consolidado_general_despa
end type
type dw_tica from datawindow within w_info_consolidado_general_despa
end type
type cbx_tica from checkbox within w_info_consolidado_general_despa
end type
type st_6 from statictext within w_info_consolidado_general_despa
end type
type st_7 from statictext within w_info_consolidado_general_despa
end type
type dw_status from datawindow within w_info_consolidado_general_despa
end type
type cbx_status from checkbox within w_info_consolidado_general_despa
end type
type cbx_constatus from checkbox within w_info_consolidado_general_despa
end type
type uo_selespecie from uo_seleccion_especie within w_info_consolidado_general_despa
end type
type st_13 from statictext within w_info_consolidado_general_despa
end type
type cbx_varirotula from checkbox within w_info_consolidado_general_despa
end type
type st_12 from statictext within w_info_consolidado_general_despa
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_consolidado_general_despa
end type
type cbx_excel from checkbox within w_info_consolidado_general_despa
end type
type dw_1 from datawindow within w_info_consolidado_general_despa
end type
end forward

global type w_info_consolidado_general_despa from w_para_informes
integer width = 3154
integer height = 2392
st_1 st_1
dw_cliente dw_cliente
st_2 st_2
st_3 st_3
st_8 st_8
cbx_planta cbx_planta
dw_planta dw_planta
st_9 st_9
st_10 st_10
st_11 st_11
dw_tiposalida dw_tiposalida
st_plantadestino st_plantadestino
dw_plantadestino dw_plantadestino
st_22 st_22
em_desde em_desde
st_23 st_23
em_hasta em_hasta
st_21 st_21
st_5 st_5
dw_transp dw_transp
cbx_trans cbx_trans
st_4 st_4
dw_tica dw_tica
cbx_tica cbx_tica
st_6 st_6
st_7 st_7
dw_status dw_status
cbx_status cbx_status
cbx_constatus cbx_constatus
uo_selespecie uo_selespecie
st_13 st_13
cbx_varirotula cbx_varirotula
st_12 st_12
uo_selproductor uo_selproductor
cbx_excel cbx_excel
dw_1 dw_1
end type
global w_info_consolidado_general_despa w_info_consolidado_general_despa

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_embarque,idwc_operaciones,idwc_transp,idwc_tica,idwc_status,&
                     idwc_planta, idwc_productor, idwc_tiposalida, idwc_plantaDestino							

Integer	ii_Cliente,  ii_Planta, ii_Operacion, ii_agrupa,ii_PlantaDestino
String	is_NomEmbarque,is_NomPlanta, is_NomCliente,is_NomPlantaDestino
Long		ii_productor
Date		id_FechaZarpe


uo_transportista    					iuo_transportista		  
uo_tipocamion							iuo_tipocamion	
uo_status								iuo_status
uo_seleccion_especie					iuo_selespecie
uo_seleccion_varios_productores	iuo_selproductor

end variables

forward prototypes
public function boolean existeproductor (long productor)
end prototypes

public function boolean existeproductor (long productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores
	WHERE	prod_codigo	=	:Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	ii_productor = Productor
	RETURN True
END IF
end function

on w_info_consolidado_general_despa.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.st_3=create st_3
this.st_8=create st_8
this.cbx_planta=create cbx_planta
this.dw_planta=create dw_planta
this.st_9=create st_9
this.st_10=create st_10
this.st_11=create st_11
this.dw_tiposalida=create dw_tiposalida
this.st_plantadestino=create st_plantadestino
this.dw_plantadestino=create dw_plantadestino
this.st_22=create st_22
this.em_desde=create em_desde
this.st_23=create st_23
this.em_hasta=create em_hasta
this.st_21=create st_21
this.st_5=create st_5
this.dw_transp=create dw_transp
this.cbx_trans=create cbx_trans
this.st_4=create st_4
this.dw_tica=create dw_tica
this.cbx_tica=create cbx_tica
this.st_6=create st_6
this.st_7=create st_7
this.dw_status=create dw_status
this.cbx_status=create cbx_status
this.cbx_constatus=create cbx_constatus
this.uo_selespecie=create uo_selespecie
this.st_13=create st_13
this.cbx_varirotula=create cbx_varirotula
this.st_12=create st_12
this.uo_selproductor=create uo_selproductor
this.cbx_excel=create cbx_excel
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_8
this.Control[iCurrent+6]=this.cbx_planta
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_9
this.Control[iCurrent+9]=this.st_10
this.Control[iCurrent+10]=this.st_11
this.Control[iCurrent+11]=this.dw_tiposalida
this.Control[iCurrent+12]=this.st_plantadestino
this.Control[iCurrent+13]=this.dw_plantadestino
this.Control[iCurrent+14]=this.st_22
this.Control[iCurrent+15]=this.em_desde
this.Control[iCurrent+16]=this.st_23
this.Control[iCurrent+17]=this.em_hasta
this.Control[iCurrent+18]=this.st_21
this.Control[iCurrent+19]=this.st_5
this.Control[iCurrent+20]=this.dw_transp
this.Control[iCurrent+21]=this.cbx_trans
this.Control[iCurrent+22]=this.st_4
this.Control[iCurrent+23]=this.dw_tica
this.Control[iCurrent+24]=this.cbx_tica
this.Control[iCurrent+25]=this.st_6
this.Control[iCurrent+26]=this.st_7
this.Control[iCurrent+27]=this.dw_status
this.Control[iCurrent+28]=this.cbx_status
this.Control[iCurrent+29]=this.cbx_constatus
this.Control[iCurrent+30]=this.uo_selespecie
this.Control[iCurrent+31]=this.st_13
this.Control[iCurrent+32]=this.cbx_varirotula
this.Control[iCurrent+33]=this.st_12
this.Control[iCurrent+34]=this.uo_selproductor
this.Control[iCurrent+35]=this.cbx_excel
this.Control[iCurrent+36]=this.dw_1
end on

on w_info_consolidado_general_despa.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.cbx_planta)
destroy(this.dw_planta)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.dw_tiposalida)
destroy(this.st_plantadestino)
destroy(this.dw_plantadestino)
destroy(this.st_22)
destroy(this.em_desde)
destroy(this.st_23)
destroy(this.em_hasta)
destroy(this.st_21)
destroy(this.st_5)
destroy(this.dw_transp)
destroy(this.cbx_trans)
destroy(this.st_4)
destroy(this.dw_tica)
destroy(this.cbx_tica)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.dw_status)
destroy(this.cbx_status)
destroy(this.cbx_constatus)
destroy(this.uo_selespecie)
destroy(this.st_13)
destroy(this.cbx_varirotula)
destroy(this.st_12)
destroy(this.uo_selproductor)
destroy(this.cbx_excel)
destroy(this.dw_1)
end on

event open;call super::open;String	ls_Columna[]
Boolean	lb_Cerrar

iuo_transportista			=	CREATE	uo_transportista       
iuo_tipocamion				=	CREATE   uo_tipocamion		
iuo_status					=	CREATE	uo_status

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.Object.clie_codigo[1]	=	gi_CodExport

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

dw_plantadestino.GetChild("plde_codigo", idwc_plantadestino)
idwc_plantadestino.SetTransObject(SQLCA)
idwc_plantadestino.Retrieve(1)
dw_plantadestino.InsertRow(0)

// uo_seleccion_productor
IF IsNull(uo_selproductor.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	//Close(This)
	lb_Cerrar = False
ELSE
	uo_selproductor.Seleccion(True,True)
END IF

dw_tiposalida.GetChild("defe_tiposa", idwc_tiposalida)
idwc_tiposalida.SetTransObject(SQLCA)
idwc_tiposalida.Retrieve()
dw_tiposalida.InsertRow(0)

dw_transp.GetChild("tran_codigo", idwc_transp)
idwc_transp.SetTransObject(SQLCA)
idwc_transp.Retrieve()
dw_transp.InsertRow(0)

dw_tica.GetChild("tica_codigo", idwc_tica)
idwc_tica.SetTransObject(SQLCA)
idwc_tica.Retrieve()
dw_tica.InsertRow(0)

dw_status.GetChild("stat_codigo",idwc_status)
idwc_status.SetTransObject(SQLCA)
idwc_status.Retrieve()
dw_status.InsertRow(0)

ExisteEspecie(gi_CodExport, gi_CodEspecie, ls_Columna[])

em_desde.Text					=	String(RelativeDate(Today(), -365))
em_hasta.Text					=	String(Today())
istr_mant.argumento[13]		=	em_desde.Text
istr_mant.argumento[14]		=	em_hasta.Text

ii_Cliente						=	gi_CodExport
ii_Planta						=	0
ii_plantadestino				=	0
istr_mant.argumento[1]		= 	String(gi_codexport)		//	Cliente
//istr_mant.argumento[2]		= 	String(gi_codespecie)	//	Especie
//istr_mant.argumento[3]		=	'0'							// Variedad
istr_mant.argumento[4]		=	'0'							// Productor
istr_mant.argumento[5]		= 	'0'							//	Planta
istr_mant.argumento[7] 		= 	String(Today())			// Fecha
istr_mant.argumento[10] 	= 	'-1'							// Transportista
istr_mant.argumento[11] 	= 	'-1'							// Tipocamion
istr_mant.argumento[20]		=	"11"
istr_mant.argumento[21]		=	"-9"
is_NomPlanta					=	"Todas"
is_NomEmbarque					=	"Todos"
//is_NomEspecie					=	ls_Columna[1]
is_NomPlantaDestino			=	""
end event

type pb_excel from w_para_informes`pb_excel within w_info_consolidado_general_despa
end type

type st_computador from w_para_informes`st_computador within w_info_consolidado_general_despa
end type

type st_usuario from w_para_informes`st_usuario within w_info_consolidado_general_despa
end type

type st_temporada from w_para_informes`st_temporada within w_info_consolidado_general_despa
end type

type p_logo from w_para_informes`p_logo within w_info_consolidado_general_despa
end type

type st_titulo from w_para_informes`st_titulo within w_info_consolidado_general_despa
integer width = 2345
string text = "Informe Consolidado Despachos Especiales"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_consolidado_general_despa
integer x = 2743
integer y = 1476
integer taborder = 110
end type

event pb_acepta::clicked;Long	  	ll_Fila, li_varirotula, ll_cont
String	t_fecha,texto_desde, texto_hasta, texto_fecha,ls_transpor,ls_tipocamion, ls_nomtipo,&
			as_Columna, ls_lista, ls_Archivo
Date 		ld_fechaZarpe, ld_desde, ld_hasta

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME DESPACHOS GENERALES ESPECIALES'

OpenWithParm(vinf, istr_info)
//vinf.dw_1.DataObject = "dw_info_consolidado_general_despa"
vinf.dw_1.DataObject = "dw_info_consolidado_general_despastatus"
vinf.dw_1.SetTransObject(sqlca)

/*
Especies
*/
If IsNull(uo_selespecie.Codigo)Then
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	Return
End If

/*
productor
*/
ls_lista = uo_selproductor.Lista

If cbx_varirotula.Checked Then
	li_varirotula = 1
Else
	li_varirotula = 0
End If

ld_desde			=	Date(istr_mant.argumento[13])
ld_hasta			=	Date(istr_mant.argumento[14])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

If cbx_trans.Checked  Then
	ls_transpor = 'Todos'
Else
	ls_transpor = iuo_transportista.nombre
End If

If cbx_tica.Checked  Then
	ls_tipocamion = 'Todos'
Else
	ls_tipocamion = iuo_tipocamion.nombre
End If

ii_Operacion	=	 Integer(istr_mant.argumento[20])

ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente, ii_Operacion, uo_selespecie.Codigo, ii_Planta, ii_PlantaDestino, &
										 ld_desde, ld_hasta, Integer(istr_mant.argumento[10]), &
										 Integer(istr_mant.argumento[11]),Integer(istr_mant.argumento[21]),&
										 li_varirotula,ls_lista)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
	StopSign!, Ok!)
Else
	as_Columna = is_NomEmbarque
	CHOOSE CASE as_Columna
		CASE "2"
			ls_nomtipo = 'Reproceso'
		CASE "3"
			ls_nomtipo = 'Reembalaje'
		CASE "5"
			ls_nomtipo = 'Reproceso Serv. 3º'
		CASE "6"
			ls_nomtipo = 'Reembalaje Serv. 3º'
		CASE "7"
			ls_nomtipo = 'Embarque Maritimo'
		CASE "8"
			ls_nomtipo = 'Embarque Aereo'
		CASE "9"
			ls_nomtipo = 'Embarque Terrestre'
		CASE "10"
			ls_nomtipo = 'Devolución al Productor'
		CASE "11"
			ls_nomtipo = 'Traspaso Inter-Planta'
		CASE "12"
			ls_nomtipo = 'M/I Cta. Propia'
		CASE "13"
			ls_nomtipo = 'M/I Cta. Productor'
		CASE "14"
			ls_nomtipo = 'Muestra Ensayo'
		CASE "15"
			ls_nomtipo = 'Botadero'	
		CASE "16"
			ls_nomtipo = 'Ventas a Terceros'
		CASE "17"
			ls_nomtipo = 'Desp. Servicios 3º'
		CASE "20"
			ls_nomtipo = 'Packing Externo'
		CASE "21"
			ls_nomtipo = 'Venta Exp. País'	
		CASE "31"
			ls_nomtipo = 'Despacho por Cajas'	
	End CHOOSE
		
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.ModIfy("planta.text = '" + is_NomPlanta + "'")
	vinf.dw_1.ModIfy("especie.text = '" + uo_selespecie.Nombre + "'")
	vinf.dw_1.ModIfy("tiposa.text = '" + ls_nomtipo + "'")
	vinf.dw_1.ModIfy("plantadestino.text = '" + is_NomPlantaDestino + "'")
	vinf.dw_1.ModIfy("fechas.text = '" + texto_fecha + "'")
	vinf.dw_1.ModIfy("transportista.text = '" + ls_transpor+ "'")
	vinf.dw_1.ModIfy("tipocamion.text = '" + ls_tipocamion + "'")
	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
	If cbx_excel.Checked Then	
		ls_Archivo	=	"\despachosespeciales.xls"
//		RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
//		GetFolder ( "selecccione carpeta", ls_Ruta)
										 
		dw_1.SetTransObject(sqlca)
		ll_cont = dw_1.Retrieve(ii_Cliente, ii_Operacion, uo_selespecie.Codigo, &
										 ii_Planta, ii_PlantaDestino, ld_desde, ld_hasta, Integer(istr_mant.argumento[10]), &
										 Integer(istr_mant.argumento[11]),Integer(istr_mant.argumento[21]),&
										 li_varirotula,ls_lista)	
		If ll_cont > 0 Then
			dw_1.SaveAs(/*ls_Ruta + ls_Archivo*/'' ,Excel5!, True)	
		End If
	End If
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_consolidado_general_despa
integer x = 2743
integer y = 1828
integer taborder = 130
end type

type st_1 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 440
integer width = 2345
integer height = 704
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_consolidado_general_despa
integer x = 741
integer y = 464
integer width = 1157
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer li_nula

SetNull(li_nula)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	ii_cliente	=	integer(data)
	
	uo_selproductor.Filtra(-1,-1,Integer(data))
	
ELSE
	This.SetItem(1, "clie_codigo", li_nula)
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 476
integer width = 270
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

type st_3 from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 644
integer width = 270
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 748
integer width = 270
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
string text = "Planta"
boolean focusrectangle = false
end type

type cbx_planta from checkbox within w_info_consolidado_general_despa
integer x = 1755
integer y = 732
integer width = 402
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	ii_Planta			=	0
	dw_planta.Enabled	=	False
	dw_planta.Reset()
	dw_planta.InsertRow(0)
	is_nomplanta		=	'Todas'
ELSE
	dw_planta.Enabled =True
	dw_planta.SetFocus()
END IF
end event

type dw_planta from datawindow within w_info_consolidado_general_despa
integer x = 741
integer y = 732
integer width = 983
integer height = 92
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExistePlanta(ii_Cliente,Integer(data), ls_Columna[]) THEN
	ii_Planta		=	Integer(data)
	is_NomPlanta	=	ls_Columna[1]
ELSE
	This.SetItem(1, "plde_codigo", li_Nula)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_9 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 1416
integer width = 2345
integer height = 260
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 1456
integer width = 347
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
string text = "Tipo Salida"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 1144
integer width = 2345
integer height = 272
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_tiposalida from datawindow within w_info_consolidado_general_despa
integer x = 741
integer y = 1432
integer width = 1079
integer height = 92
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "ddlbdw_tiposalida"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

istr_mant.argumento[20]	=	data

IF Integer(istr_mant.argumento[20]) <> 11 THEN
	dw_plantadestino.visible	=	False
	dw_plantadestino.Enabled	=	False
	st_plantadestino.visible	=	False
ELSE
	dw_plantadestino.visible	=	True
	dw_plantadestino.Enabled 	=	True
	dw_plantadestino.SetFocus()
	st_plantadestino.visible	=	True
END IF


IF Not IsNull(data) AND Integer(data) <> 0  THEN
	is_NomEmbarque	=	String(data)
ELSE
	//This.SetItem(1, "defe_codigo", li_Nula)	
	RETURN 1
END IF
end event

type st_plantadestino from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 1556
integer width = 434
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
string text = "Planta Destino"
boolean focusrectangle = false
end type

type dw_plantadestino from datawindow within w_info_consolidado_general_despa
integer x = 741
integer y = 1548
integer width = 974
integer height = 92
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExistePlanta(ii_Cliente,Integer(data), ls_Columna[]) THEN
	ii_PlantaDestino		=	Integer(data)
	is_NomPlantaDestino	=	ls_Columna[1]
ELSE
	This.SetItem(1, "plde_codigo", li_Nula)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_22 from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 1996
integer width = 425
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
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_consolidado_general_despa
integer x = 741
integer y = 1980
integer width = 393
integer height = 96
integer taborder = 80
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

event modified;istr_mant.argumento[13]	=	This.Text
end event

type st_23 from statictext within w_info_consolidado_general_despa
integer x = 1189
integer y = 1996
integer width = 297
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
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_consolidado_general_despa
integer x = 1527
integer y = 1980
integer width = 393
integer height = 96
integer taborder = 100
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

event modified;istr_mant.argumento[14]	=	This.Text
end event

type st_21 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 1960
integer width = 2345
integer height = 148
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 1192
integer width = 425
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
string text = "Transportista"
boolean focusrectangle = false
end type

type dw_transp from datawindow within w_info_consolidado_general_despa
integer x = 745
integer y = 1168
integer width = 896
integer height = 92
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_transportista"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula
SetNull(li_nula)

IF iuo_transportista.existe(Integer(data),True,sqlca) THEN
	istr_mant.argumento[10] = data
ELSE
	This.SetItem(1, "tran_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_trans from checkbox within w_info_consolidado_general_despa
integer x = 1755
integer y = 1188
integer width = 256
integer height = 72
integer taborder = 150
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
	istr_mant.argumento[10]	=	'-1'
	dw_transp.Enabled	=	False
	dw_transp.Reset()
	dw_transp.InsertRow(0)
ELSE
	dw_transp.Enabled	=	True
	dw_transp.SetFocus()
END IF
end event

type st_4 from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 1296
integer width = 389
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
string text = "Tipo Camión"
boolean focusrectangle = false
end type

type dw_tica from datawindow within w_info_consolidado_general_despa
integer x = 741
integer y = 1288
integer width = 983
integer height = 92
integer taborder = 180
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_tipocamion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula
SetNull(li_nula)

IF iuo_tipocamion.existe(Integer(data),True,sqlca) THEN
	istr_mant.argumento[11] = data
ELSE
	This.SetItem(1, "tica_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_tica from checkbox within w_info_consolidado_general_despa
integer x = 1755
integer y = 1288
integer width = 256
integer height = 68
integer taborder = 160
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
	istr_mant.argumento[11]	=	'-1'
	dw_tica.Enabled	=	False
	dw_tica.Reset()
	dw_tica.InsertRow(0)
ELSE
	dw_tica.Enabled	=	True
	dw_tica.SetFocus()
END IF
end event

type st_6 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 1676
integer width = 2345
integer height = 148
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 1716
integer width = 402
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
string text = "Status"
boolean focusrectangle = false
end type

type dw_status from datawindow within w_info_consolidado_general_despa
integer x = 741
integer y = 1700
integer width = 1010
integer height = 108
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula
SetNull(li_nula)

IF iuo_status.existe(Integer(data),True,sqlca) THEN
	istr_mant.argumento[21] = data
ELSE
	This.SetItem(1, "stat_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_status from checkbox within w_info_consolidado_general_despa
integer x = 1755
integer y = 1712
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_status.Enabled		=	False
	cbx_constatus.Enabled=	False
	istr_mant.argumento[21]	=	'-1'
ELSE
	dw_status.Enabled		=	True
	dw_status.Reset()
	dw_status.InsertRow(0)
	dw_status.SetFocus()
	cbx_constatus.Enabled=	True
END IF	

end event

type cbx_constatus from checkbox within w_info_consolidado_general_despa
integer x = 2085
integer y = 1712
integer width = 443
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
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.checked=True THEN
	dw_status.Enabled=False
	istr_mant.argumento[21]	=	'-9'
ELSE
	istr_mant.argumento[21]	=	'-1'
	dw_status.Enabled=True
	dw_status.Reset()
	dw_status.InsertRow(0)
	dw_status.SetFocus()
END IF
	
end event

type uo_selespecie from uo_seleccion_especie within w_info_consolidado_general_despa
event destroy ( )
integer x = 731
integer y = 556
integer height = 180
integer taborder = 90
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_13 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 1824
integer width = 2345
integer height = 136
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_varirotula from checkbox within w_info_consolidado_general_despa
integer x = 741
integer y = 1852
integer width = 745
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
string text = "Variedad Rotulada"
end type

type st_12 from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 984
integer width = 297
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
string text = "Productor"
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_consolidado_general_despa
integer x = 745
integer y = 844
integer taborder = 120
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

type cbx_excel from checkbox within w_info_consolidado_general_despa
integer x = 2039
integer y = 1996
integer width = 521
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
string text = "Archivo Excel"
boolean lefttext = true
end type

type dw_1 from datawindow within w_info_consolidado_general_despa
boolean visible = false
integer x = 2702
integer y = 320
integer width = 261
integer height = 208
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_archivodespacho_especiales"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

