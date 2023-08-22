$PBExportHeader$w_info_pallet_recepcionados.srw
forward
global type w_info_pallet_recepcionados from w_para_informes
end type
type st_1 from statictext within w_info_pallet_recepcionados
end type
type dw_cliente from datawindow within w_info_pallet_recepcionados
end type
type st_6 from statictext within w_info_pallet_recepcionados
end type
type dw_planta from datawindow within w_info_pallet_recepcionados
end type
type st_3 from statictext within w_info_pallet_recepcionados
end type
type st_variedad from statictext within w_info_pallet_recepcionados
end type
type st_4 from statictext within w_info_pallet_recepcionados
end type
type st_productor from statictext within w_info_pallet_recepcionados
end type
type cbx_productor from checkbox within w_info_pallet_recepcionados
end type
type em_productor from editmask within w_info_pallet_recepcionados
end type
type cb_buscaproductor from commandbutton within w_info_pallet_recepcionados
end type
type sle_productor from singlelineedit within w_info_pallet_recepcionados
end type
type st_2 from statictext within w_info_pallet_recepcionados
end type
type em_desde from editmask within w_info_pallet_recepcionados
end type
type st_7 from statictext within w_info_pallet_recepcionados
end type
type em_hasta from editmask within w_info_pallet_recepcionados
end type
type cbx_planta from checkbox within w_info_pallet_recepcionados
end type
type st_12 from statictext within w_info_pallet_recepcionados
end type
type dw_stat from datawindow within w_info_pallet_recepcionados
end type
type cbx_consolid from checkbox within w_info_pallet_recepcionados
end type
type st_55 from statictext within w_info_pallet_recepcionados
end type
type st_5 from statictext within w_info_pallet_recepcionados
end type
type uo_selcondicion from uo_seleccion_condicion within w_info_pallet_recepcionados
end type
type st_8 from statictext within w_info_pallet_recepcionados
end type
type em_ncajas from editmask within w_info_pallet_recepcionados
end type
type cbx_pallets from checkbox within w_info_pallet_recepcionados
end type
type st_9 from statictext within w_info_pallet_recepcionados
end type
type cbx_ccalidad from checkbox within w_info_pallet_recepcionados
end type
type uo_selespecie from uo_seleccion_especie within w_info_pallet_recepcionados
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_pallet_recepcionados
end type
type cbx_varirotula from checkbox within w_info_pallet_recepcionados
end type
type rb_apallet from radiobutton within w_info_pallet_recepcionados
end type
type rb_pucho from radiobutton within w_info_pallet_recepcionados
end type
type rb_todospall from radiobutton within w_info_pallet_recepcionados
end type
type gb_3 from groupbox within w_info_pallet_recepcionados
end type
end forward

global type w_info_pallet_recepcionados from w_para_informes
integer x = 14
integer y = 32
integer width = 2715
integer height = 2280
string title = "Resumen De Pallet Recepcionados"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
st_variedad st_variedad
st_4 st_4
st_productor st_productor
cbx_productor cbx_productor
em_productor em_productor
cb_buscaproductor cb_buscaproductor
sle_productor sle_productor
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
cbx_planta cbx_planta
st_12 st_12
dw_stat dw_stat
cbx_consolid cbx_consolid
st_55 st_55
st_5 st_5
uo_selcondicion uo_selcondicion
st_8 st_8
em_ncajas em_ncajas
cbx_pallets cbx_pallets
st_9 st_9
cbx_ccalidad cbx_ccalidad
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
rb_apallet rb_apallet
rb_pucho rb_pucho
rb_todospall rb_todospall
gb_3 gb_3
end type
global w_info_pallet_recepcionados w_info_pallet_recepcionados

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_etiqueta, idwc_stat, idwc_especie, idwc_variedad

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad

end variables

forward prototypes
public function boolean noexistestatus (integer ia_codigo)
end prototypes

public function boolean noexistestatus (integer ia_codigo);Integer	li_existe
boolean lb_retorno
SELECT	count(*)
	INTO	:li_existe
	FROM	dbo.status
	WHERE	stat_codigo	= :ia_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla ClientesProd")
	lb_retorno = TRUE
ELSEIF li_existe > 0 THEN	
	lb_retorno = FALSE
ELSE
	lb_retorno = TRUE
END IF

RETURN lb_retorno
end function

on w_info_pallet_recepcionados.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_4=create st_4
this.st_productor=create st_productor
this.cbx_productor=create cbx_productor
this.em_productor=create em_productor
this.cb_buscaproductor=create cb_buscaproductor
this.sle_productor=create sle_productor
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.cbx_planta=create cbx_planta
this.st_12=create st_12
this.dw_stat=create dw_stat
this.cbx_consolid=create cbx_consolid
this.st_55=create st_55
this.st_5=create st_5
this.uo_selcondicion=create uo_selcondicion
this.st_8=create st_8
this.em_ncajas=create em_ncajas
this.cbx_pallets=create cbx_pallets
this.st_9=create st_9
this.cbx_ccalidad=create cbx_ccalidad
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.rb_apallet=create rb_apallet
this.rb_pucho=create rb_pucho
this.rb_todospall=create rb_todospall
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.st_productor
this.Control[iCurrent+9]=this.cbx_productor
this.Control[iCurrent+10]=this.em_productor
this.Control[iCurrent+11]=this.cb_buscaproductor
this.Control[iCurrent+12]=this.sle_productor
this.Control[iCurrent+13]=this.st_2
this.Control[iCurrent+14]=this.em_desde
this.Control[iCurrent+15]=this.st_7
this.Control[iCurrent+16]=this.em_hasta
this.Control[iCurrent+17]=this.cbx_planta
this.Control[iCurrent+18]=this.st_12
this.Control[iCurrent+19]=this.dw_stat
this.Control[iCurrent+20]=this.cbx_consolid
this.Control[iCurrent+21]=this.st_55
this.Control[iCurrent+22]=this.st_5
this.Control[iCurrent+23]=this.uo_selcondicion
this.Control[iCurrent+24]=this.st_8
this.Control[iCurrent+25]=this.em_ncajas
this.Control[iCurrent+26]=this.cbx_pallets
this.Control[iCurrent+27]=this.st_9
this.Control[iCurrent+28]=this.cbx_ccalidad
this.Control[iCurrent+29]=this.uo_selespecie
this.Control[iCurrent+30]=this.uo_selvariedad
this.Control[iCurrent+31]=this.cbx_varirotula
this.Control[iCurrent+32]=this.rb_apallet
this.Control[iCurrent+33]=this.rb_pucho
this.Control[iCurrent+34]=this.rb_todospall
this.Control[iCurrent+35]=this.gb_3
end on

on w_info_pallet_recepcionados.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_4)
destroy(this.st_productor)
destroy(this.cbx_productor)
destroy(this.em_productor)
destroy(this.cb_buscaproductor)
destroy(this.sle_productor)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.cbx_planta)
destroy(this.st_12)
destroy(this.dw_stat)
destroy(this.cbx_consolid)
destroy(this.st_55)
destroy(this.st_5)
destroy(this.uo_selcondicion)
destroy(this.st_8)
destroy(this.em_ncajas)
destroy(this.cbx_pallets)
destroy(this.st_9)
destroy(this.cbx_ccalidad)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.rb_apallet)
destroy(this.rb_pucho)
destroy(this.rb_todospall)
destroy(this.gb_3)
end on

event open;call super::open;/* Argumentos :
					 [1] = Cliente
					 [2] = Planta
					 [3] = Especie
					 [4] = Variedad
*/

x	=	0
y	=	0

Boolean lb_Cerrar

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
	uo_selvariedad.Enabled		=	False
END IF

IF IsNull(uo_selcondicion.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selcondicion.Seleccion(True, True)
	uo_selcondicion.cbx_consolida.Visible = FALSE
	uo_selcondicion.cbx_consolida.Enabled = FALSE

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
	
	dw_stat.GetChild("stat_codigo", idwc_stat)
	idwc_stat.SetTransObject(SQLCA)
	idwc_stat.Retrieve()
	dw_stat.InsertRow(0)
	
	istr_mant.argumento[25]	= "0"
	istr_mant.argumento[26]	= "Consolidado"	
	dw_stat.Object.stat_codigo.background.color = RGB(166,180,210)
	
	em_desde.Text				=	String(RelativeDate(Today(), -365))
	em_hasta.Text				=	String(Today())
	
	istr_mant.argumento[1]	= 	String(gi_CodExport)
	istr_mant.argumento[2]	=	'-1'
	istr_mant.argumento[5]	=	'0'
	istr_mant.argumento[6]	=	'Consolidados'
	istr_mant.argumento[23]	= 	em_desde.Text
	istr_mant.argumento[24]	=	em_hasta.Text
	istr_mant.argumento[27]	= "0"					// Cantidad de cajas por pallets
	
	em_ncajas.Enabled				=	False
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_pallet_recepcionados
end type

type st_computador from w_para_informes`st_computador within w_info_pallet_recepcionados
end type

type st_usuario from w_para_informes`st_usuario within w_info_pallet_recepcionados
end type

type st_temporada from w_para_informes`st_temporada within w_info_pallet_recepcionados
end type

type p_logo from w_para_informes`p_logo within w_info_pallet_recepcionados
end type

type st_titulo from w_para_informes`st_titulo within w_info_pallet_recepcionados
integer width = 1934
string text = "Resumen De Pallet Recepcionados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_pallet_recepcionados
string tag = "Imprimir Reporte"
integer x = 2359
integer y = 1412
integer taborder = 180
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila,li_planta,li_cliente, li_condicion, li_ncajas, li_ccalidad, li_varirotula,li_tipallet
			
String  	ls_planta,ls_especie, ls_cliente, ls_productor, ls_condicion	
Date		ld_desde, ld_hasta
Long		ll_productor

istr_info.titulo	= 'INFORME DE PALLET RECEPCIONADOS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_resumen_pallet_recepcionados"
vinf.dw_1.SetTransObject(sqlca)

IF cbx_planta.Checked THEN
	ls_Planta   =  'Todas'
ELSE
   ls_planta	=	idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")
END IF

IF cbx_consolid.checked THEN
	istr_mant.argumento[25]	= "0"
	istr_mant.argumento[26]	= "Consolidado"
END IF	

IF cbx_ccalidad.checked THEN
	li_ccalidad = 1
ELSE
	li_ccalidad = 0
END IF	

IF rb_apallet.checked THEN
	li_tipallet = 1
END IF	
IF rb_pucho.checked THEN
	li_tipallet = 2
END IF	
IF rb_todospall.checked THEN
	li_tipallet = -1
END IF	

//Condición
IF uo_selcondicion.cbx_todos.checked THEN
   li_condicion	= -1
	ls_condicion   = 'Todas'
ELSE
   li_condicion	= uo_selcondicion.dw_Seleccion.Object.codigo[1]
	IF IsNull(li_condicion)THEN
	   MessageBox("Atención","Debe Seleccionar una Condición Previamente",Exclamation!)
	   RETURN
	ELSE
		ls_condicion   = uo_selcondicion.Nombre
	END IF
END IF
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

ll_productor 	= 	Long(istr_mant.argumento[5])
ls_productor	=	istr_mant.argumento[6]

vinf.dw_1.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()

li_cliente 		=	Integer(istr_mant.argumento[1])

SELECT clie_nombre
INTO   :ls_Cliente
FROM dbo.clientesprod
where clie_codigo = :li_Cliente;

li_planta		=	Integer(istr_mant.argumento[2])
ld_desde		=	Date(istr_mant.argumento[23])
ld_hasta		=	Date(istr_mant.argumento[24])
li_ncajas		=	Integer(istr_mant.argumento[27])

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, uo_selespecie.Codigo,&
			uo_selvariedad.Codigo,ll_productor,ld_desde,ld_hasta,&
			integer(istr_mant.argumento[25]),li_condicion,li_ncajas,li_ccalidad,li_varirotula,li_tipallet)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	vinf.dw_1.Modify("planta.text = '" + ls_planta + "'")
	vinf.dw_1.Modify("cliente.text = '" + ls_cliente + "'")
	vinf.dw_1.Modify("t_status.text = '" + Upper(istr_mant.argumento[26]) + "'")
	vinf.dw_1.Modify("productor.text = '" + String(ll_productor,'#####')+' '+ls_productor + "'")
	vinf.dw_1.Modify("desde.text = '" + String(ld_desde)+"'")
	vinf.dw_1.Modify("hasta.text = '" + String(ld_hasta)+"'")	
	vinf.dw_1.Modify("fumigados.text = '" + ls_condicion+"'")		
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_pallet_recepcionados
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2359
integer y = 1712
integer taborder = 190
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_pallet_recepcionados
integer x = 338
integer y = 592
integer width = 215
integer height = 76
boolean bringtotop = true
integer textsize = -8
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

type dw_cliente from datawindow within w_info_pallet_recepcionados
integer x = 768
integer y = 464
integer width = 1147
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_pallet_recepcionados
integer x = 338
integer y = 492
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type dw_planta from datawindow within w_info_pallet_recepcionados
integer x = 768
integer y = 572
integer width = 965
integer height = 96
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_pallet_recepcionados
integer x = 338
integer y = 940
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_pallet_recepcionados
integer x = 338
integer y = 1128
integer width = 279
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_pallet_recepcionados
integer x = 251
integer y = 440
integer width = 1934
integer height = 1324
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

type st_productor from statictext within w_info_pallet_recepcionados
integer x = 338
integer y = 780
integer width = 297
integer height = 56
boolean bringtotop = true
integer textsize = -8
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

type cbx_productor from checkbox within w_info_pallet_recepcionados
integer x = 768
integer y = 680
integer width = 471
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
	istr_mant.argumento[5]		=	'0'
	istr_mant.argumento[6]		=	'Consolidados'
ELSE
	em_productor.Enabled			=	True
	cb_buscaproductor.Enabled	=	True
END IF
end event

type em_productor from editmask within w_info_pallet_recepcionados
integer x = 768
integer y = 756
integer width = 233
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;String		ls_Nombre
Long			ll_productor
Integer		li_cliente

IF This.Text <> '' AND This.Text <> '0' THEN
	ll_productor	=	Long(This.Text)
	li_cliente 		=  dw_cliente.Object.clie_codigo[1]
	
	SELECT	prod_nombre
		INTO	:ls_Nombre
		FROM	dbo.productores as pro,dbo.productoresclientes as cli
		WHERE	pro.prod_codigo =	:ll_productor
		AND	pro.prod_codigo = cli.prod_codigo
		AND	:li_cliente in (-1,cli.clie_codigo);
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla Productores")
		This.SetFocus()
	ELSEIF sqlca.SQLCode = 100 THEN
	
		MessageBox("Atención", "Código de productor no ha sido definido o no pertenece a este cliente.~r~r" + &
			"Ingrese o seleccione otro Código.")
		em_productor.Text 	= ''
		sle_productor.Text 	= ''
		This.SetFocus()
	ELSE
		sle_productor.Text		=	ls_Nombre
		istr_mant.argumento[4]	=	String(ll_productor)
	END IF
END IF	
end event

type cb_buscaproductor from commandbutton within w_info_pallet_recepcionados
integer x = 1010
integer y = 764
integer width = 96
integer height = 84
integer taborder = 70
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

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_productores_clientes, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[3] = "" THEN
	em_productor.SetFocus()
ELSE
	em_productor.Text			=	lstr_busq.argum[3]
	sle_productor.Text		=	lstr_busq.argum[4]
	istr_mant.argumento[5]	=	lstr_busq.argum[3]
	istr_mant.argumento[6]	=	lstr_busq.argum[4]
END IF
end event

type sle_productor from singlelineedit within w_info_pallet_recepcionados
integer x = 1115
integer y = 756
integer width = 987
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_info_pallet_recepcionados
integer x = 343
integer y = 1784
integer width = 283
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_pallet_recepcionados
integer x = 773
integer y = 1784
integer width = 416
integer height = 96
integer taborder = 160
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

event modified;istr_mant.argumento[23]	=	This.Text
end event

type st_7 from statictext within w_info_pallet_recepcionados
integer x = 1211
integer y = 1784
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type em_hasta from editmask within w_info_pallet_recepcionados
integer x = 1646
integer y = 1784
integer width = 416
integer height = 96
integer taborder = 170
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

event modified;istr_mant.argumento[24]	=	This.Text
end event

type cbx_planta from checkbox within w_info_pallet_recepcionados
integer x = 1778
integer y = 584
integer width = 279
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
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

event clicked;Integer	li_planta

SetNull(li_planta)
IF This.Checked THEN
	dw_planta.Enabled				=	False
	dw_planta.SetItem(1,"plde_codigo",li_planta)
	istr_mant.argumento[2]		=	'-1'
ELSE
	dw_planta.Enabled				=	True
	dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)
   istr_mant.argumento[2]	= 	String(gi_CodPlanta)
END IF
end event

type st_12 from statictext within w_info_pallet_recepcionados
integer x = 338
integer y = 1316
integer width = 265
integer height = 80
boolean bringtotop = true
integer textsize = -8
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

type dw_stat from datawindow within w_info_pallet_recepcionados
integer x = 759
integer y = 1316
integer width = 969
integer height = 88
integer taborder = 130
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF  NoExisteStatus(integer(data)) THEN
	MessageBox("informe","No Existe Status")
	dw_stat.SetItem(1,"stat_codigo",li_Null)
	RETURN 1
ELSE	
	istr_mant.argumento[25]	= data
	istr_mant.argumento[26]	=  f_statnombre(integer(data))
END IF

end event

type cbx_consolid from checkbox within w_info_pallet_recepcionados
integer x = 768
integer y = 1240
integer width = 471
integer height = 80
integer taborder = 120
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.checked THEN
	dw_stat.enabled = FALSE
	dw_stat.Object.stat_codigo.background.color = RGB(166,180,210)
ELSE
	dw_stat.enabled = TRUE
	dw_stat.Object.stat_codigo.background.color = rgb(255,255,255)
	istr_mant.argumento[25]	= "1"	
	istr_mant.argumento[26]	=  f_statnombre(1)
	dw_stat.Object.stat_codigo[1] = 1
END IF	
end event

type st_55 from statictext within w_info_pallet_recepcionados
integer x = 251
integer y = 1764
integer width = 1934
integer height = 140
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

type st_5 from statictext within w_info_pallet_recepcionados
integer x = 338
integer y = 1520
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Condición"
boolean focusrectangle = false
end type

type uo_selcondicion from uo_seleccion_condicion within w_info_pallet_recepcionados
integer x = 773
integer y = 1440
integer height = 172
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcondicion.destroy
call uo_seleccion_condicion::destroy
end on

type st_8 from statictext within w_info_pallet_recepcionados
integer x = 338
integer y = 1636
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Pallets de "
boolean focusrectangle = false
end type

type em_ncajas from editmask within w_info_pallet_recepcionados
integer x = 773
integer y = 1636
integer width = 393
integer height = 96
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "###"
end type

event modified;istr_mant.argumento[27]	=	This.Text
end event

type cbx_pallets from checkbox within w_info_pallet_recepcionados
integer x = 1221
integer y = 1636
integer width = 279
integer height = 80
integer taborder = 140
boolean bringtotop = true
integer textsize = -8
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

event clicked;
IF This.Checked THEN
	em_ncajas.Enabled				=	False
	em_ncajas.Text					=	""	
	istr_mant.argumento[27]		=	'0'
ELSE
	em_ncajas.Enabled				=	True
	istr_mant.argumento[27]		=	em_ncajas.Text
	em_ncajas.Setfocus()
END IF


end event

type st_9 from statictext within w_info_pallet_recepcionados
integer x = 251
integer y = 1904
integer width = 1934
integer height = 112
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

type cbx_ccalidad from checkbox within w_info_pallet_recepcionados
boolean visible = false
integer x = 1929
integer y = 2264
integer width = 745
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Por Control de Calidad"
end type

type uo_selespecie from uo_seleccion_especie within w_info_pallet_recepcionados
event destroy ( )
integer x = 768
integer y = 864
integer height = 180
integer taborder = 50
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
		uo_selvariedad.Enabled						=	False	
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
		uo_selvariedad.Enabled						=	True
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_pallet_recepcionados
event destroy ( )
integer x = 768
integer y = 1048
integer taborder = 60
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_pallet_recepcionados
boolean visible = false
integer x = 1925
integer y = 2360
integer width = 745
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Variedad Rotulada"
end type

type rb_apallet from radiobutton within w_info_pallet_recepcionados
integer x = 535
integer y = 1924
integer width = 283
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Pallet"
end type

event clicked;istr_mant.argumento[33]	=	'1'
end event

type rb_pucho from radiobutton within w_info_pallet_recepcionados
integer x = 1029
integer y = 1924
integer width = 302
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Puchos"
boolean checked = true
end type

event clicked;istr_mant.argumento[33]	=	'2'
end event

type rb_todospall from radiobutton within w_info_pallet_recepcionados
integer x = 1481
integer y = 1924
integer width = 370
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
end type

event clicked;istr_mant.argumento[33]	=	'-1'
end event

type gb_3 from groupbox within w_info_pallet_recepcionados
integer x = 315
integer y = 1872
integer width = 1609
integer height = 148
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

