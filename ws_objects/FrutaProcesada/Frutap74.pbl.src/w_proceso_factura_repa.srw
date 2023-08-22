$PBExportHeader$w_proceso_factura_repa.srw
forward
global type w_proceso_factura_repa from w_para_informes
end type
type st_1 from statictext within w_proceso_factura_repa
end type
type st_2 from statictext within w_proceso_factura_repa
end type
type em_desde from editmask within w_proceso_factura_repa
end type
type dw_cliente from datawindow within w_proceso_factura_repa
end type
type st_6 from statictext within w_proceso_factura_repa
end type
type dw_planta from datawindow within w_proceso_factura_repa
end type
type st_7 from statictext within w_proceso_factura_repa
end type
type em_hasta from editmask within w_proceso_factura_repa
end type
type st_5 from statictext within w_proceso_factura_repa
end type
type dw_1 from datawindow within w_proceso_factura_repa
end type
type rb_todos from radiobutton within w_proceso_factura_repa
end type
type rb_pda from radiobutton within w_proceso_factura_repa
end type
type cbx_todos from checkbox within w_proceso_factura_repa
end type
type gb_3 from groupbox within w_proceso_factura_repa
end type
type st_4 from statictext within w_proceso_factura_repa
end type
end forward

global type w_proceso_factura_repa from w_para_informes
integer x = 14
integer y = 32
integer width = 3218
integer height = 1960
string title = "MARCA FACTURABLE REPALLETIZADO"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_7 st_7
em_hasta em_hasta
st_5 st_5
dw_1 dw_1
rb_todos rb_todos
rb_pda rb_pda
cbx_todos cbx_todos
gb_3 gb_3
st_4 st_4
end type
global w_proceso_factura_repa w_proceso_factura_repa

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

li_cliente = dw_cliente.Object.clie_codigo[1]

SELECT	pro.prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores as pro,dbo.productoresclientes as cli
	WHERE	pro.prod_codigo	=	:ll_Productor 
	AND pro.prod_codigo = cli.prod_codigo
	AND cli.clie_codigo = :li_cliente;
	
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
FROM	dbo.plantadesp
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

on w_proceso_factura_repa.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_5=create st_5
this.dw_1=create dw_1
this.rb_todos=create rb_todos
this.rb_pda=create rb_pda
this.cbx_todos=create cbx_todos
this.gb_3=create gb_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.dw_cliente
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_planta
this.Control[iCurrent+7]=this.st_7
this.Control[iCurrent+8]=this.em_hasta
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.dw_1
this.Control[iCurrent+11]=this.rb_todos
this.Control[iCurrent+12]=this.rb_pda
this.Control[iCurrent+13]=this.cbx_todos
this.Control[iCurrent+14]=this.gb_3
this.Control[iCurrent+15]=this.st_4
end on

on w_proceso_factura_repa.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_5)
destroy(this.dw_1)
destroy(this.rb_todos)
destroy(this.rb_pda)
destroy(this.cbx_todos)
destroy(this.gb_3)
destroy(this.st_4)
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
dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)

em_desde.Text				=	String(Today())//String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	sTRING(gi_CodPlanta)							//	planta despachadora
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_proceso_factura_repa
boolean visible = true
integer x = 2834
integer y = 744
boolean enabled = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
end type

event pb_excel::clicked;call super::clicked;Integer fila, li_cliente, li_planta
Date		ld_desde, ld_hasta
String	ls_pda

pb_acepta.Enabled = False

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
ld_desde			=	Date(istr_mant.argumento[3])
ld_hasta			=	Date(istr_mant.argumento[4])

IF rb_todos.Checked THEN
	ls_pda = '-1'
ELSE
	ls_pda = 'Repa por PDA'
END IF	

dw_1.SetTransObject(sqlca)

fila	=	dw_1.Retrieve(li_cliente,li_planta, 	ld_desde,ld_hasta,ls_pda)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	pb_acepta.Enabled = False
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para estos filtros.", &
	             StopSign!, Ok!)
	pb_acepta.Enabled = False					 

ELSE
	pb_acepta.Enabled = True
END IF
end event

type st_computador from w_para_informes`st_computador within w_proceso_factura_repa
boolean visible = false
integer x = 3081
integer y = 1632
end type

type st_usuario from w_para_informes`st_usuario within w_proceso_factura_repa
boolean visible = false
integer x = 3081
integer y = 1560
end type

type st_temporada from w_para_informes`st_temporada within w_proceso_factura_repa
boolean visible = false
integer x = 3081
integer y = 1488
end type

type p_logo from w_para_informes`p_logo within w_proceso_factura_repa
end type

type st_titulo from w_para_informes`st_titulo within w_proceso_factura_repa
integer x = 242
integer width = 2487
string text = "Marca Repalletizados Facturables"
end type

type pb_acepta from w_para_informes`pb_acepta within w_proceso_factura_repa
integer x = 2843
integer y = 1036
integer taborder = 150
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
end type

event pb_acepta::clicked;Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, w_proceso_factura_repa.Title)
		MessageBox("Problema", "Grabación NO Realizada.")
	ELSE
					
		dw_1.ResetUpdate()
		
		MessageBox("Atención", "Grabación Satisfactoria.")
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, w_proceso_factura_repa.Title)
	MessageBox("Problema", "Grabación NO Realizada.")
END IF

sqlca.AutoCommit	=	lb_AutoCommit


end event

type pb_salir from w_para_informes`pb_salir within w_proceso_factura_repa
integer x = 2843
integer y = 1324
integer taborder = 170
end type

type st_1 from statictext within w_proceso_factura_repa
integer x = 389
integer y = 584
integer width = 293
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

type st_2 from statictext within w_proceso_factura_repa
integer x = 389
integer y = 692
integer width = 384
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
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_proceso_factura_repa
integer x = 928
integer y = 676
integer width = 393
integer height = 96
integer taborder = 130
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

event modified;istr_mant.argumento[3]	=	This.Text
end event

type dw_cliente from datawindow within w_proceso_factura_repa
integer x = 923
integer y = 488
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
	
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_proceso_factura_repa
integer x = 389
integer y = 488
integer width = 306
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

type dw_planta from datawindow within w_proceso_factura_repa
integer x = 923
integer y = 584
integer width = 1175
integer height = 92
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantas"
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

type st_7 from statictext within w_proceso_factura_repa
integer x = 1390
integer y = 692
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_proceso_factura_repa
integer x = 1664
integer y = 676
integer width = 393
integer height = 96
integer taborder = 140
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

event modified;istr_mant.argumento[4]	=	This.Text
end event

type st_5 from statictext within w_proceso_factura_repa
integer x = 242
integer y = 888
integer width = 2487
integer height = 916
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

type dw_1 from datawindow within w_proceso_factura_repa
integer x = 265
integer y = 900
integer width = 2441
integer height = 880
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_proceso_mant_factura_repa"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type rb_todos from radiobutton within w_proceso_factura_repa
integer x = 923
integer y = 788
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

type rb_pda from radiobutton within w_proceso_factura_repa
integer x = 1650
integer y = 788
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
string text = "Solo PDA"
end type

type cbx_todos from checkbox within w_proceso_factura_repa
integer x = 2331
integer y = 756
integer width = 279
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
end type

event clicked;LOng	ll_fila

IF This.Checked THEN
	FOR ll_fila = 1 TO dw_1.RowCount()
		dw_1.Object.repe_factur[ll_fila] = 1
	NEXT
ELSE
	FOR ll_fila = 1 TO dw_1.RowCount()
		dw_1.Object.repe_factur[ll_fila] = 0
	NEXT	
END IF	
end event

type gb_3 from groupbox within w_proceso_factura_repa
integer x = 2222
integer y = 684
integer width = 480
integer height = 176
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Marca"
end type

type st_4 from statictext within w_proceso_factura_repa
integer x = 242
integer y = 448
integer width = 2487
integer height = 436
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

