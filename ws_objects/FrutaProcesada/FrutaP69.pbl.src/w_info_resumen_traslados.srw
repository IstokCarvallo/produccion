$PBExportHeader$w_info_resumen_traslados.srw
forward
global type w_info_resumen_traslados from w_para_informes
end type
type st_4 from statictext within w_info_resumen_traslados
end type
type st_1 from statictext within w_info_resumen_traslados
end type
type dw_cliente from datawindow within w_info_resumen_traslados
end type
type st_6 from statictext within w_info_resumen_traslados
end type
type dw_planta from datawindow within w_info_resumen_traslados
end type
type tit_peso from statictext within w_info_resumen_traslados
end type
type st_9 from statictext within w_info_resumen_traslados
end type
type st_10 from statictext within w_info_resumen_traslados
end type
type dw_operaciones from datawindow within w_info_resumen_traslados
end type
type gb_3 from groupbox within w_info_resumen_traslados
end type
end forward

global type w_info_resumen_traslados from w_para_informes
integer x = 14
integer y = 32
integer width = 2523
integer height = 1412
string title = "Resumen de la Operación"
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
tit_peso tit_peso
st_9 st_9
st_10 st_10
dw_operaciones dw_operaciones
gb_3 gb_3
end type
global w_info_resumen_traslados w_info_resumen_traslados

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie, idwc_productor, idwc_packing, idwc_pesoneto,&
						idwc_operaciones,idwc_embarque, idwc_tipocamion, idwc_transporte,idwc_puertos, idwc_tiposalida

String 	is_NomPlanta, is_Embarque, is_Operacion, is_NomEmbarque
Integer	ii_Operacion, ii_transportista
Long		ll_norden

uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad
uo_seleccion_varios_productores	iuo_selproductor


end variables

on w_info_resumen_traslados.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.tit_peso=create tit_peso
this.st_9=create st_9
this.st_10=create st_10
this.dw_operaciones=create dw_operaciones
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_cliente
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.tit_peso
this.Control[iCurrent+7]=this.st_9
this.Control[iCurrent+8]=this.st_10
this.Control[iCurrent+9]=this.dw_operaciones
this.Control[iCurrent+10]=this.gb_3
end on

on w_info_resumen_traslados.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.tit_peso)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.dw_operaciones)
destroy(this.gb_3)
end on

event open;call super::open;x	=	0
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
dw_planta.SetItem(1, "plde_codigo", gi_Codplanta)

dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
idwc_operaciones.SetTransObject(SQLCA)
idwc_operaciones.Retrieve(gi_CodExport)
dw_operaciones.InsertRow(0)

istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	String(gi_Codplanta)			//	planta
//istr_mant.argumento[5]	= 	"-9"							//	operacion





end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_resumen_traslados
end type

type st_computador from w_para_informes`st_computador within w_info_resumen_traslados
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_traslados
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_traslados
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_traslados
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_traslados
integer width = 1787
string text = "Resumen de la Operación"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_traslados
string tag = "Imprimir Reporte"
integer x = 2181
integer y = 648
integer taborder = 140
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta
			
istr_info.titulo	= 'RESUMEN DE LA OPERACION'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_resumen_traslados_camiones"  

li_cliente 		=	Integer(istr_mant.argumento[1])				// cliente
li_planta		=	Integer(istr_mant.argumento[2])				//	planta
//ii_operacion	=	Integer(istr_mant.argumento[5])				//	operacion

IF ii_Operacion = 0 THEN
	MessageBox( "Atención", "Falta Selecionar Operación.", &
	             StopSign!, Ok!)
	dw_operaciones.SetFocus()					 
	Return					 
END IF	

IF li_planta = 0 THEN
	MessageBox( "Atención", "Falta Selecionar Planta.", &
	             StopSign!, Ok!)
	dw_planta.SetFocus()	
	Return
END IF	

vinf.dw_1.SetTransObject(sqlca)
fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta,ii_operacion)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_traslados
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2185
integer y = 944
integer taborder = 150
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_resumen_traslados
integer x = 251
integer y = 440
integer width = 1787
integer height = 712
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

type st_1 from statictext within w_info_resumen_traslados
integer x = 375
integer y = 712
integer width = 229
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

type dw_cliente from datawindow within w_info_resumen_traslados
integer x = 786
integer y = 512
integer width = 1175
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

type st_6 from statictext within w_info_resumen_traslados
integer x = 375
integer y = 536
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

type dw_planta from datawindow within w_info_resumen_traslados
integer x = 786
integer y = 712
integer width = 974
integer height = 92
integer taborder = 40
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

type tit_peso from statictext within w_info_resumen_traslados
boolean visible = false
integer x = 567
integer y = 2008
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

type st_9 from statictext within w_info_resumen_traslados
boolean visible = false
integer x = 2190
integer y = 1956
integer width = 238
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Packing"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_resumen_traslados
integer x = 375
integer y = 908
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
string text = "Operacion"
boolean focusrectangle = false
end type

type dw_operaciones from datawindow within w_info_resumen_traslados
integer x = 786
integer y = 908
integer width = 974
integer height = 92
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_operacion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExisteOperacion(Integer(istr_mant.argumento[1]), Integer(data), ls_Columna[]) THEN
	ii_Operacion	=	Integer(data)
	istr_mant.argumento[5] = data
END IF
end event

event itemerror;RETURN 1
end event

type gb_3 from groupbox within w_info_resumen_traslados
boolean visible = false
integer x = 1627
integer y = 1964
integer width = 1614
integer height = 280
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

