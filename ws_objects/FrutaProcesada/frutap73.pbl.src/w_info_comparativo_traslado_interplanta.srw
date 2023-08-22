$PBExportHeader$w_info_comparativo_traslado_interplanta.srw
forward
global type w_info_comparativo_traslado_interplanta from w_para_informes
end type
type st_4 from statictext within w_info_comparativo_traslado_interplanta
end type
type st_1 from statictext within w_info_comparativo_traslado_interplanta
end type
type st_2 from statictext within w_info_comparativo_traslado_interplanta
end type
type em_desde from editmask within w_info_comparativo_traslado_interplanta
end type
type dw_cliente from datawindow within w_info_comparativo_traslado_interplanta
end type
type st_6 from statictext within w_info_comparativo_traslado_interplanta
end type
type dw_planta from datawindow within w_info_comparativo_traslado_interplanta
end type
type st_3 from statictext within w_info_comparativo_traslado_interplanta
end type
type cbx_planta from checkbox within w_info_comparativo_traslado_interplanta
end type
type st_7 from statictext within w_info_comparativo_traslado_interplanta
end type
type em_hasta from editmask within w_info_comparativo_traslado_interplanta
end type
type st_8 from statictext within w_info_comparativo_traslado_interplanta
end type
type dw_productor from datawindow within w_info_comparativo_traslado_interplanta
end type
type cbx_productor from checkbox within w_info_comparativo_traslado_interplanta
end type
type st_5 from statictext within w_info_comparativo_traslado_interplanta
end type
type st_15 from statictext within w_info_comparativo_traslado_interplanta
end type
type st_variedad from statictext within w_info_comparativo_traslado_interplanta
end type
type dw_packing from datawindow within w_info_comparativo_traslado_interplanta
end type
type cbx_packing from checkbox within w_info_comparativo_traslado_interplanta
end type
type st_9 from statictext within w_info_comparativo_traslado_interplanta
end type
type st_10 from statictext within w_info_comparativo_traslado_interplanta
end type
type em_nguia from editmask within w_info_comparativo_traslado_interplanta
end type
type cbx_1 from checkbox within w_info_comparativo_traslado_interplanta
end type
type uo_selespecie from uo_seleccion_especie within w_info_comparativo_traslado_interplanta
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_comparativo_traslado_interplanta
end type
type cbx_varirotula from checkbox within w_info_comparativo_traslado_interplanta
end type
end forward

global type w_info_comparativo_traslado_interplanta from w_para_informes
integer x = 14
integer y = 32
integer width = 2702
integer height = 2416
string title = "COMPARATIVO TRASLADOS INTERPLANTA"
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
dw_productor dw_productor
cbx_productor cbx_productor
st_5 st_5
st_15 st_15
st_variedad st_variedad
dw_packing dw_packing
cbx_packing cbx_packing
st_9 st_9
st_10 st_10
em_nguia em_nguia
cbx_1 cbx_1
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
end type
global w_info_comparativo_traslado_interplanta w_info_comparativo_traslado_interplanta

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor, idwc_packing, idwc_pesoneto

String is_NomPlanta

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
end variables

forward prototypes
public function boolean existeproductor (integer li_cliente, long ll_productor)
public function boolean existepacking (integer li_cliente)
end prototypes

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

public function boolean existepacking (integer li_cliente);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
WHERE	plde_codigo =  :li_cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[7] = String(li_cliente)
	RETURN True 
END IF


end function

on w_info_comparativo_traslado_interplanta.create
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
this.dw_productor=create dw_productor
this.cbx_productor=create cbx_productor
this.st_5=create st_5
this.st_15=create st_15
this.st_variedad=create st_variedad
this.dw_packing=create dw_packing
this.cbx_packing=create cbx_packing
this.st_9=create st_9
this.st_10=create st_10
this.em_nguia=create em_nguia
this.cbx_1=create cbx_1
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
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
this.Control[iCurrent+13]=this.dw_productor
this.Control[iCurrent+14]=this.cbx_productor
this.Control[iCurrent+15]=this.st_5
this.Control[iCurrent+16]=this.st_15
this.Control[iCurrent+17]=this.st_variedad
this.Control[iCurrent+18]=this.dw_packing
this.Control[iCurrent+19]=this.cbx_packing
this.Control[iCurrent+20]=this.st_9
this.Control[iCurrent+21]=this.st_10
this.Control[iCurrent+22]=this.em_nguia
this.Control[iCurrent+23]=this.cbx_1
this.Control[iCurrent+24]=this.uo_selespecie
this.Control[iCurrent+25]=this.uo_selvariedad
this.Control[iCurrent+26]=this.cbx_varirotula
end on

on w_info_comparativo_traslado_interplanta.destroy
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
destroy(this.dw_productor)
destroy(this.cbx_productor)
destroy(this.st_5)
destroy(this.st_15)
destroy(this.st_variedad)
destroy(this.dw_packing)
destroy(this.cbx_packing)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.em_nguia)
destroy(this.cbx_1)
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

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(1)
dw_packing.InsertRow(0)

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
istr_mant.argumento[2]	= 	"-1"							//	planta despachadora
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
//istr_mant.argumento[5]	=	"-1"							//	especie
istr_mant.argumento[6]  =  "-1"							//	productor
istr_mant.argumento[7]  =  "-1"							//	packing  planta receptora
istr_mant.argumento[8]	= 	"-1" 							// Guías
//istr_mant.argumento[9]	= 	"-1" 							// variedad
end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_comparativo_traslado_interplanta
end type

type st_usuario from w_para_informes`st_usuario within w_info_comparativo_traslado_interplanta
end type

type st_temporada from w_para_informes`st_temporada within w_info_comparativo_traslado_interplanta
end type

type p_logo from w_para_informes`p_logo within w_info_comparativo_traslado_interplanta
end type

type st_titulo from w_para_informes`st_titulo within w_info_comparativo_traslado_interplanta
integer width = 1938
string text = "Comparativo Traslados Interplantas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_comparativo_traslado_interplanta
integer x = 2281
integer y = 1688
integer taborder = 150
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer fila, li_cliente, li_planta_d, li_planta_r, li_varirotula
Long		ll_productor, ll_nguia
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha

istr_info.titulo	= 'INFORME COMPARATIVO TRASLADOS INTERPLANTA'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_comparativo_traslado_interplanta"

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
li_planta_d		=	Integer(istr_mant.argumento[2])
li_planta_r		=	Integer(istr_mant.argumento[7])
ll_productor	=	Long(istr_mant.argumento[6])
ld_desde			=	Date(istr_mant.argumento[3])
ld_hasta			=	Date(istr_mant.argumento[4])
ll_nguia   		=  Long(istr_mant.argumento[8])

texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente,li_planta_d,li_planta_r,ll_productor, &
						 			 uo_selespecie.Codigo,uo_selvariedad.Codigo,ld_desde,&
									 ld_hasta,ll_nguia,li_varirotula)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("Fechas.text = '" + texto_fecha + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_comparativo_traslado_interplanta
integer x = 2281
integer y = 1976
integer taborder = 170
end type

type st_4 from statictext within w_info_comparativo_traslado_interplanta
integer x = 247
integer y = 440
integer width = 1938
integer height = 752
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

type st_1 from statictext within w_info_comparativo_traslado_interplanta
integer x = 302
integer y = 704
integer width = 635
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
string text = "Planta Despachadora"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_comparativo_traslado_interplanta
integer x = 302
integer y = 1992
integer width = 384
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Desde Desp."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_comparativo_traslado_interplanta
integer x = 960
integer y = 1992
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

type dw_cliente from datawindow within w_info_comparativo_traslado_interplanta
integer x = 960
integer y = 524
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

type st_6 from statictext within w_info_comparativo_traslado_interplanta
integer x = 302
integer y = 524
integer width = 306
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

type dw_planta from datawindow within w_info_comparativo_traslado_interplanta
integer x = 960
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

type st_3 from statictext within w_info_comparativo_traslado_interplanta
integer x = 302
integer y = 1328
integer width = 306
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

type cbx_planta from checkbox within w_info_comparativo_traslado_interplanta
integer x = 960
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
	istr_mant.argumento[2]	=	'-1'
ELSE
	dw_planta.Enabled			=	True
	dw_planta.SetFocus()
END IF
end event

type st_7 from statictext within w_info_comparativo_traslado_interplanta
integer x = 1422
integer y = 1992
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_comparativo_traslado_interplanta
integer x = 1696
integer y = 1992
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

type st_8 from statictext within w_info_comparativo_traslado_interplanta
integer x = 302
integer y = 1084
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

type dw_productor from datawindow within w_info_comparativo_traslado_interplanta
integer x = 960
integer y = 1084
integer width = 974
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

event itemchanged;istr_mant.argumento[6]	=	data	

end event

type cbx_productor from checkbox within w_info_comparativo_traslado_interplanta
integer x = 960
integer y = 1000
integer width = 402
integer height = 76
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

event clicked;IF This.Checked = TRUE THEN
	dw_productor.Enabled  = False
	dw_productor.Reset()
	dw_productor.insertrow(0)
	istr_mant.argumento[6]	=	'-1'
ELSE
	dw_productor.Enabled  = True
	dw_productor.SetFocus()
	dw_productor.Reset()
	dw_productor.InsertRow(0)
END IF
	
end event

type st_5 from statictext within w_info_comparativo_traslado_interplanta
integer x = 247
integer y = 1196
integer width = 1938
integer height = 528
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

type st_15 from statictext within w_info_comparativo_traslado_interplanta
integer x = 247
integer y = 1728
integer width = 1938
integer height = 456
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

type st_variedad from statictext within w_info_comparativo_traslado_interplanta
integer x = 302
integer y = 1504
integer width = 306
integer height = 64
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

type dw_packing from datawindow within w_info_comparativo_traslado_interplanta
integer x = 960
integer y = 892
integer width = 965
integer height = 100
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	 ls_null
SetNull(ls_null)

IF ExistePacking(Integer(data))THEN
	istr_mant.argumento[7]	=	data		
	RETURN 0
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

type cbx_packing from checkbox within w_info_comparativo_traslado_interplanta
integer x = 960
integer y = 812
integer width = 402
integer height = 80
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked = True THEN
	dw_packing.Enabled = False
	istr_mant.argumento[7]	=	'-1'
ELSE
	dw_packing.Enabled=True	
END IF	
end event

type st_9 from statictext within w_info_comparativo_traslado_interplanta
integer x = 302
integer y = 892
integer width = 635
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
string text = "Planta Receptora"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_comparativo_traslado_interplanta
integer x = 302
integer y = 1856
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Nro. Guía"
boolean focusrectangle = false
end type

type em_nguia from editmask within w_info_comparativo_traslado_interplanta
integer x = 960
integer y = 1856
integer width = 393
integer height = 96
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[8]	=	This.Text
end event

type cbx_1 from checkbox within w_info_comparativo_traslado_interplanta
integer x = 960
integer y = 1764
integer width = 402
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_nguia.Enabled				=	False
	em_nguia.Text					=	""	
	istr_mant.argumento[8]		=	'-1'
ELSE
	em_nguia.Enabled				=	True
	istr_mant.argumento[8]		=	em_nguia.Text
	em_nguia.Setfocus()
END IF



end event

type uo_selespecie from uo_seleccion_especie within w_info_comparativo_traslado_interplanta
event destroy ( )
integer x = 960
integer y = 1228
integer height = 180
integer taborder = 270
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

type uo_selvariedad from uo_seleccion_variedad within w_info_comparativo_traslado_interplanta
event destroy ( )
integer x = 960
integer y = 1408
integer taborder = 260
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_comparativo_traslado_interplanta
integer x = 965
integer y = 1608
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

