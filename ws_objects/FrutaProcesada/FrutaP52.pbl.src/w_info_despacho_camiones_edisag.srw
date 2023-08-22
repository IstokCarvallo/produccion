$PBExportHeader$w_info_despacho_camiones_edisag.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_despacho_camiones_edisag from w_para_informes
end type
type st_1 from statictext within w_info_despacho_camiones_edisag
end type
type dw_cliente from datawindow within w_info_despacho_camiones_edisag
end type
type st_2 from statictext within w_info_despacho_camiones_edisag
end type
type st_8 from statictext within w_info_despacho_camiones_edisag
end type
type dw_planta from datawindow within w_info_despacho_camiones_edisag
end type
type st_9 from statictext within w_info_despacho_camiones_edisag
end type
type st_10 from statictext within w_info_despacho_camiones_edisag
end type
type dw_operaciones from datawindow within w_info_despacho_camiones_edisag
end type
type st_12 from statictext within w_info_despacho_camiones_edisag
end type
type dw_tipocamion from datawindow within w_info_despacho_camiones_edisag
end type
type st_13 from statictext within w_info_despacho_camiones_edisag
end type
type cbx_patente from checkbox within w_info_despacho_camiones_edisag
end type
type cbx_2 from checkbox within w_info_despacho_camiones_edisag
end type
type st_3 from statictext within w_info_despacho_camiones_edisag
end type
type em_desde from editmask within w_info_despacho_camiones_edisag
end type
type st_7 from statictext within w_info_despacho_camiones_edisag
end type
type em_hasta from editmask within w_info_despacho_camiones_edisag
end type
type st_4 from statictext within w_info_despacho_camiones_edisag
end type
type cbx_3 from checkbox within w_info_despacho_camiones_edisag
end type
type em_patente from editmask within w_info_despacho_camiones_edisag
end type
type cbx_1 from checkbox within w_info_despacho_camiones_edisag
end type
end forward

global type w_info_despacho_camiones_edisag from w_para_informes
integer width = 2944
integer height = 1768
st_1 st_1
dw_cliente dw_cliente
st_2 st_2
st_8 st_8
dw_planta dw_planta
st_9 st_9
st_10 st_10
dw_operaciones dw_operaciones
st_12 st_12
dw_tipocamion dw_tipocamion
st_13 st_13
cbx_patente cbx_patente
cbx_2 cbx_2
st_3 st_3
em_desde em_desde
st_7 st_7
em_hasta em_hasta
st_4 st_4
cbx_3 cbx_3
em_patente em_patente
cbx_1 cbx_1
end type
global w_info_despacho_camiones_edisag w_info_despacho_camiones_edisag

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_especie, idwc_embarque,idwc_operaciones,&
                     idwc_planta, idwc_productor, idwc_tipocamion

Integer	ii_Cliente, ii_Especie, ii_Planta, ii_Operacion, ii_agrupa,ii_tipocamion
String	is_Embarque, is_NomEspecie, is_NomEmbarque, is_NomNave, is_NomPlanta, &
			is_NomCliente, is_OPeracion
Long		ii_productor

Date		id_FechaZarpe


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

on w_info_despacho_camiones_edisag.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.st_8=create st_8
this.dw_planta=create dw_planta
this.st_9=create st_9
this.st_10=create st_10
this.dw_operaciones=create dw_operaciones
this.st_12=create st_12
this.dw_tipocamion=create dw_tipocamion
this.st_13=create st_13
this.cbx_patente=create cbx_patente
this.cbx_2=create cbx_2
this.st_3=create st_3
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_4=create st_4
this.cbx_3=create cbx_3
this.em_patente=create em_patente
this.cbx_1=create cbx_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.st_9
this.Control[iCurrent+7]=this.st_10
this.Control[iCurrent+8]=this.dw_operaciones
this.Control[iCurrent+9]=this.st_12
this.Control[iCurrent+10]=this.dw_tipocamion
this.Control[iCurrent+11]=this.st_13
this.Control[iCurrent+12]=this.cbx_patente
this.Control[iCurrent+13]=this.cbx_2
this.Control[iCurrent+14]=this.st_3
this.Control[iCurrent+15]=this.em_desde
this.Control[iCurrent+16]=this.st_7
this.Control[iCurrent+17]=this.em_hasta
this.Control[iCurrent+18]=this.st_4
this.Control[iCurrent+19]=this.cbx_3
this.Control[iCurrent+20]=this.em_patente
this.Control[iCurrent+21]=this.cbx_1
end on

on w_info_despacho_camiones_edisag.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.st_8)
destroy(this.dw_planta)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.dw_operaciones)
destroy(this.st_12)
destroy(this.dw_tipocamion)
destroy(this.st_13)
destroy(this.cbx_patente)
destroy(this.cbx_2)
destroy(this.st_3)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_4)
destroy(this.cbx_3)
destroy(this.em_patente)
destroy(this.cbx_1)
end on

event open;String	ls_Columna[]

x	=	0
Y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.Object.clie_codigo[1]	=	gi_CodExport

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.Object.plde_codigo[1]	=	gi_CodPlanta

dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
idwc_operaciones.SetTransObject(SQLCA)
idwc_operaciones.Retrieve(gi_CodExport)
dw_operaciones.InsertRow(0)

dw_tipocamion.GetChild("tica_codigo", idwc_tipocamion)
idwc_tipocamion.SetTransObject(SQLCA)
idwc_tipocamion.Retrieve()
dw_tipocamion.InsertRow(0)
dw_tipocamion.Object.tica_codigo[1]	=	2

ii_Cliente						=	gi_CodExport
ii_Planta						=	gi_CodPlanta
ii_operacion			     	=  0
ii_tipocamion					=	2

em_desde.Text					=	String(RelativeDate(Today(), -365))
em_hasta.Text					=	String(Today())
em_desde.Enabled				=	False
em_hasta.Enabled				=	False	
em_patente.Enabled			=	False	
istr_mant.argumento[10]		= '%'
	
end event

type pb_excel from w_para_informes`pb_excel within w_info_despacho_camiones_edisag
end type

type st_computador from w_para_informes`st_computador within w_info_despacho_camiones_edisag
end type

type st_usuario from w_para_informes`st_usuario within w_info_despacho_camiones_edisag
end type

type st_temporada from w_para_informes`st_temporada within w_info_despacho_camiones_edisag
end type

type p_logo from w_para_informes`p_logo within w_info_despacho_camiones_edisag
end type

type st_titulo from w_para_informes`st_titulo within w_info_despacho_camiones_edisag
string text = "Informe Despacho Camiones por Operación"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_despacho_camiones_edisag
string tag = "Imprimir Reporte"
integer x = 2446
integer y = 828
integer taborder = 110
end type

event pb_acepta::clicked;Long	  	ll_Fila
String		t_fecha, ls_operacion, ls_fechas
Date 		ld_fechaZarpe

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME DESPACHO DE CAMIONES EDI-SAG'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_despacho_camiones_edisag2"
vinf.dw_1.SetTransObject(sqlca)

IF cbx_2.Checked THEN
	ls_operacion	=	'Todas'
ELSE
	ls_operacion	=	String(ii_Operacion)
END IF

ls_fechas	=	String(Date(em_desde.Text))+' al '+ String(Date(em_hasta.Text))

ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente, ii_Planta, ii_Operacion, ii_Tipocamion,Date(em_desde.Text),Date(em_hasta.Text),istr_mant.argumento[10])

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("operacion.text = '" + ls_Operacion + "'")
	vinf.dw_1.Modify("fechas.text = '" + ls_fechas + "'")		
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF
end event

type pb_salir from w_para_informes`pb_salir within w_info_despacho_camiones_edisag
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2446
integer y = 1108
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_despacho_camiones_edisag
integer x = 247
integer y = 440
integer width = 2039
integer height = 276
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

type dw_cliente from datawindow within w_info_despacho_camiones_edisag
integer x = 754
integer y = 472
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
	ii_cliente	=	integer(data)

	idwc_operaciones.Retrieve(Integer(istr_mant.argumento[1]))
   ii_operacion	=	0
ELSE
	This.SetItem(1, "clie_codigo", li_nula)
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_despacho_camiones_edisag
integer x = 343
integer y = 484
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

type st_8 from statictext within w_info_despacho_camiones_edisag
integer x = 343
integer y = 600
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

type dw_planta from datawindow within w_info_despacho_camiones_edisag
integer x = 754
integer y = 592
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

type st_9 from statictext within w_info_despacho_camiones_edisag
integer x = 247
integer y = 716
integer width = 2039
integer height = 276
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

type st_10 from statictext within w_info_despacho_camiones_edisag
integer x = 343
integer y = 744
integer width = 315
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
string text = "Operación"
boolean focusrectangle = false
end type

type dw_operaciones from datawindow within w_info_despacho_camiones_edisag
integer x = 754
integer y = 744
integer width = 974
integer height = 92
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_operacion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExisteOperacion(ii_Cliente, Integer(data), ls_Columna[]) THEN
	ii_Operacion	=	Integer(data)
	is_Embarque		=	"Z"
	idwc_embarque.retrieve(ii_Cliente, ii_OPeracion)
ELSE
	This.SetItem(1, "oper_codigo", li_Nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_12 from statictext within w_info_despacho_camiones_edisag
integer x = 247
integer y = 992
integer width = 2039
integer height = 432
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

type dw_tipocamion from datawindow within w_info_despacho_camiones_edisag
integer x = 754
integer y = 864
integer width = 974
integer height = 92
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_tipocamion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;ii_Tipocamion	=	Integer(data)
end event

event itemerror;RETURN 1
end event

type st_13 from statictext within w_info_despacho_camiones_edisag
integer x = 343
integer y = 864
integer width = 325
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
string text = "Transporte"
boolean focusrectangle = false
end type

type cbx_patente from checkbox within w_info_despacho_camiones_edisag
integer x = 1774
integer y = 1196
integer width = 402
integer height = 80
integer taborder = 100
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
	em_patente.Text			=	''
	em_patente.Enabled		=	False
	istr_mant.argumento[10]	= '%'

ELSE
	em_patente.Enabled		=	True
	em_patente.SetFocus()
END IF
	
	
end event

type cbx_2 from checkbox within w_info_despacho_camiones_edisag
integer x = 1774
integer y = 744
integer width = 402
integer height = 80
integer taborder = 40
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

event clicked;IF This.Checked THEN
	dw_Operaciones.Enabled  = False
	dw_Operaciones.insertrow(0)
	ii_Operacion	=	0
ELSE
	dw_Operaciones.Enabled  = True
	ii_Operacion=dw_Operaciones.Object.oper_codigo[1]
	dw_Operaciones.SetFocus()
//	dw_Tipocamion.InsertRow(0)
END IF
	
	
end event

type st_3 from statictext within w_info_despacho_camiones_edisag
integer x = 343
integer y = 1060
integer width = 283
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
string text = "Desde "
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_despacho_camiones_edisag
integer x = 754
integer y = 1044
integer width = 393
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_7 from statictext within w_info_despacho_camiones_edisag
integer x = 1189
integer y = 1060
integer width = 197
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_despacho_camiones_edisag
integer x = 1362
integer y = 1044
integer width = 393
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_4 from statictext within w_info_despacho_camiones_edisag
integer x = 343
integer y = 1220
integer width = 343
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
string text = "Patente"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_3 from checkbox within w_info_despacho_camiones_edisag
integer x = 1774
integer y = 1044
integer width = 402
integer height = 80
integer taborder = 80
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
	em_desde.Text				=	String(RelativeDate(Today(), -365))
	em_hasta.Text				=	String(Today())
	em_desde.Enabled			=	False
	em_hasta.Enabled			=	False	
ELSE
	em_desde.Enabled			=	True
	em_hasta.Enabled			=	True	
	em_desde.SetFocus()
END IF
	
	
end event

type em_patente from editmask within w_info_despacho_camiones_edisag
integer x = 754
integer y = 1196
integer width = 393
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;istr_mant.argumento[10] = This.Text + '%'
end event

type cbx_1 from checkbox within w_info_despacho_camiones_edisag
integer x = 1774
integer y = 864
integer width = 402
integer height = 80
integer taborder = 40
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
end type

event clicked;IF This.Checked THEN
	dw_Tipocamion.Enabled  = False
	dw_Tipocamion.insertrow(0)
	ii_Tipocamion	=	0
ELSE
	dw_Tipocamion.Enabled  = True
	ii_Tipocamion=dw_Tipocamion.Object.tica_codigo[1]
	dw_Tipocamion.SetFocus()
//	dw_Tipocamion.InsertRow(0)
END IF
	
end event

