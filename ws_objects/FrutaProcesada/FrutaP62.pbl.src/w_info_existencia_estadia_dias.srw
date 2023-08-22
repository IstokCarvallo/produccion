$PBExportHeader$w_info_existencia_estadia_dias.srw
forward
global type w_info_existencia_estadia_dias from w_para_informes
end type
type dw_cliente from datawindow within w_info_existencia_estadia_dias
end type
type st_6 from statictext within w_info_existencia_estadia_dias
end type
type st_4 from statictext within w_info_existencia_estadia_dias
end type
type st_especie from statictext within w_info_existencia_estadia_dias
end type
type st_nro2 from statictext within w_info_existencia_estadia_dias
end type
type st_variedad from statictext within w_info_existencia_estadia_dias
end type
type st_productor from statictext within w_info_existencia_estadia_dias
end type
type em_productor from editmask within w_info_existencia_estadia_dias
end type
type cb_buscaproductor from commandbutton within w_info_existencia_estadia_dias
end type
type sle_productor from singlelineedit within w_info_existencia_estadia_dias
end type
type cbx_productor from checkbox within w_info_existencia_estadia_dias
end type
type st_planta from statictext within w_info_existencia_estadia_dias
end type
type cbx_planta from checkbox within w_info_existencia_estadia_dias
end type
type dw_planta from datawindow within w_info_existencia_estadia_dias
end type
type gb_5 from groupbox within w_info_existencia_estadia_dias
end type
type st_5 from statictext within w_info_existencia_estadia_dias
end type
type rb_embalaje from radiobutton within w_info_existencia_estadia_dias
end type
type rb_productor from radiobutton within w_info_existencia_estadia_dias
end type
type rb_variedad from radiobutton within w_info_existencia_estadia_dias
end type
type st_7 from statictext within w_info_existencia_estadia_dias
end type
type cbx_consolplan from checkbox within w_info_existencia_estadia_dias
end type
type cbx_consolprod from checkbox within w_info_existencia_estadia_dias
end type
type rb_controltodos from radiobutton within w_info_existencia_estadia_dias
end type
type rb_rechazados from radiobutton within w_info_existencia_estadia_dias
end type
type rb_objetados from radiobutton within w_info_existencia_estadia_dias
end type
type rb_habilitado from radiobutton within w_info_existencia_estadia_dias
end type
type gb_6 from groupbox within w_info_existencia_estadia_dias
end type
type st_1 from statictext within w_info_existencia_estadia_dias
end type
type st_2 from statictext within w_info_existencia_estadia_dias
end type
type uo_selcondicion from uo_seleccion_condicion within w_info_existencia_estadia_dias
end type
type cbx_predio from checkbox within w_info_existencia_estadia_dias
end type
type st_3 from statictext within w_info_existencia_estadia_dias
end type
type dw_stat from datawindow within w_info_existencia_estadia_dias
end type
type cbx_status from checkbox within w_info_existencia_estadia_dias
end type
type uo_selespecie from uo_seleccion_especie within w_info_existencia_estadia_dias
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_estadia_dias
end type
type cbx_varirotula from checkbox within w_info_existencia_estadia_dias
end type
end forward

global type w_info_existencia_estadia_dias from w_para_informes
integer x = 14
integer y = 32
integer width = 2747
integer height = 2320
string title = "Existencia de Fruta"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_cliente dw_cliente
st_6 st_6
st_4 st_4
st_especie st_especie
st_nro2 st_nro2
st_variedad st_variedad
st_productor st_productor
em_productor em_productor
cb_buscaproductor cb_buscaproductor
sle_productor sle_productor
cbx_productor cbx_productor
st_planta st_planta
cbx_planta cbx_planta
dw_planta dw_planta
gb_5 gb_5
st_5 st_5
rb_embalaje rb_embalaje
rb_productor rb_productor
rb_variedad rb_variedad
st_7 st_7
cbx_consolplan cbx_consolplan
cbx_consolprod cbx_consolprod
rb_controltodos rb_controltodos
rb_rechazados rb_rechazados
rb_objetados rb_objetados
rb_habilitado rb_habilitado
gb_6 gb_6
st_1 st_1
st_2 st_2
uo_selcondicion uo_selcondicion
cbx_predio cbx_predio
st_3 st_3
dw_stat dw_stat
cbx_status cbx_status
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
end type
global w_info_existencia_estadia_dias w_info_existencia_estadia_dias

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente,idwc_planta

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad

String is_NomPlanta,is_NomProductor, is_control_d
integer ii_control
end variables

forward prototypes
public function boolean noexisteplanta (string planta)
public function boolean noexistecliente (string cliente)
end prototypes

public function boolean noexisteplanta (string planta);Integer		 li_planta

li_planta	=	Integer(planta)

SELECT	plde_nombre
	INTO	:is_NomPlanta
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:li_planta;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla PlantaDesp")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	dw_planta.SetItem(1, "plde_codigo", li_planta)
	istr_mant.argumento[5]	=	String(li_planta)	
	RETURN False
END IF
end function

public function boolean noexistecliente (string cliente);Integer		li_cliente

String ls_nombre
li_cliente	=	Integer(cliente)

SELECT	clie_nombre
	INTO	:ls_nombre
	FROM	dbo.clientesprod
	WHERE	clie_codigo	=	:li_cliente;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla ClientesProd")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	dw_cliente.SetItem(1, "clie_codigo", li_cliente)
	istr_mant.argumento[1]	=	String(li_cliente)	
	RETURN False
END IF
end function

on w_info_existencia_estadia_dias.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_4=create st_4
this.st_especie=create st_especie
this.st_nro2=create st_nro2
this.st_variedad=create st_variedad
this.st_productor=create st_productor
this.em_productor=create em_productor
this.cb_buscaproductor=create cb_buscaproductor
this.sle_productor=create sle_productor
this.cbx_productor=create cbx_productor
this.st_planta=create st_planta
this.cbx_planta=create cbx_planta
this.dw_planta=create dw_planta
this.gb_5=create gb_5
this.st_5=create st_5
this.rb_embalaje=create rb_embalaje
this.rb_productor=create rb_productor
this.rb_variedad=create rb_variedad
this.st_7=create st_7
this.cbx_consolplan=create cbx_consolplan
this.cbx_consolprod=create cbx_consolprod
this.rb_controltodos=create rb_controltodos
this.rb_rechazados=create rb_rechazados
this.rb_objetados=create rb_objetados
this.rb_habilitado=create rb_habilitado
this.gb_6=create gb_6
this.st_1=create st_1
this.st_2=create st_2
this.uo_selcondicion=create uo_selcondicion
this.cbx_predio=create cbx_predio
this.st_3=create st_3
this.dw_stat=create dw_stat
this.cbx_status=create cbx_status
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_especie
this.Control[iCurrent+5]=this.st_nro2
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_productor
this.Control[iCurrent+8]=this.em_productor
this.Control[iCurrent+9]=this.cb_buscaproductor
this.Control[iCurrent+10]=this.sle_productor
this.Control[iCurrent+11]=this.cbx_productor
this.Control[iCurrent+12]=this.st_planta
this.Control[iCurrent+13]=this.cbx_planta
this.Control[iCurrent+14]=this.dw_planta
this.Control[iCurrent+15]=this.gb_5
this.Control[iCurrent+16]=this.st_5
this.Control[iCurrent+17]=this.rb_embalaje
this.Control[iCurrent+18]=this.rb_productor
this.Control[iCurrent+19]=this.rb_variedad
this.Control[iCurrent+20]=this.st_7
this.Control[iCurrent+21]=this.cbx_consolplan
this.Control[iCurrent+22]=this.cbx_consolprod
this.Control[iCurrent+23]=this.rb_controltodos
this.Control[iCurrent+24]=this.rb_rechazados
this.Control[iCurrent+25]=this.rb_objetados
this.Control[iCurrent+26]=this.rb_habilitado
this.Control[iCurrent+27]=this.gb_6
this.Control[iCurrent+28]=this.st_1
this.Control[iCurrent+29]=this.st_2
this.Control[iCurrent+30]=this.uo_selcondicion
this.Control[iCurrent+31]=this.cbx_predio
this.Control[iCurrent+32]=this.st_3
this.Control[iCurrent+33]=this.dw_stat
this.Control[iCurrent+34]=this.cbx_status
this.Control[iCurrent+35]=this.uo_selespecie
this.Control[iCurrent+36]=this.uo_selvariedad
this.Control[iCurrent+37]=this.cbx_varirotula
end on

on w_info_existencia_estadia_dias.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_4)
destroy(this.st_especie)
destroy(this.st_nro2)
destroy(this.st_variedad)
destroy(this.st_productor)
destroy(this.em_productor)
destroy(this.cb_buscaproductor)
destroy(this.sle_productor)
destroy(this.cbx_productor)
destroy(this.st_planta)
destroy(this.cbx_planta)
destroy(this.dw_planta)
destroy(this.gb_5)
destroy(this.st_5)
destroy(this.rb_embalaje)
destroy(this.rb_productor)
destroy(this.rb_variedad)
destroy(this.st_7)
destroy(this.cbx_consolplan)
destroy(this.cbx_consolprod)
destroy(this.rb_controltodos)
destroy(this.rb_rechazados)
destroy(this.rb_objetados)
destroy(this.rb_habilitado)
destroy(this.gb_6)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selcondicion)
destroy(this.cbx_predio)
destroy(this.st_3)
destroy(this.dw_stat)
destroy(this.cbx_status)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
end on

event open;call super::open;Boolean   lb_cerrar

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
	dw_cliente.SetItem(1, "clie_codigo", gi_codexport)
	
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(SQLCA)
	idwc_planta.Retrieve(1)
	dw_planta.InsertRow(0)
	dw_planta.SetItem(1, "plde_codigo", gi_codplanta)
	
	istr_mant.argumento[1]	= 	String(gi_codexport)		//	Cliente
//	istr_mant.argumento[2]	= 	String(gi_codespecie)	//	Especie
//	istr_mant.argumento[3]	=	'0'							// Variedad
	istr_mant.argumento[5]	= 	'0'							//	Planta
	istr_mant.argumento[28] = 	'0'// Consolidado por Planta
	istr_mant.argumento[27] = 	'0'// Consolidado por Productor
	istr_mant.argumento[35]  =	'0'							// status
	istr_mant.argumento[36]	=  "Todos"						// status nombre
	
	is_NomPlanta				=	"TODAS"
	ii_control 					= 	-1
	is_control_d				=	"Todos"
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_existencia_estadia_dias
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_estadia_dias
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_estadia_dias
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_estadia_dias
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_estadia_dias
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_estadia_dias
integer width = 1966
string text = "Existencia por Estadía por Días"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_estadia_dias
string tag = "Imprimir Reporte"
integer x = 2345
integer y = 1512
integer taborder = 120
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	Fila,li_Orden,li_Cond, li_varirotula
String	l_s_Titulo,ls_Condicion,ls_planta,ls_productor

l_s_Titulo	=	'Existencia por Calidad'

istr_info.titulo	=	'Fruta Procesada - ' + l_s_Titulo

//Condición
IF uo_selcondicion.cbx_todos.checked THEN
   li_Cond	= -1
	ls_condicion = 'Todas'
ELSE
   li_Cond	= uo_SelCondicion.Codigo
   IF IsNull(li_Cond)THEN
	   MessageBox("Atención","Debe Seleccionar una Condición Previamente",Exclamation!)
	   RETURN
	ELSE
		ls_condicion   = uo_SelCondicion.Nombre
	END IF
END IF

IF cbx_consolplan.CheCked THEN
	ls_Planta	=  "Consolidado"
ELSE	
   IF cbx_planta.CheCked  THEN
		ls_Planta	=  "Todos"
	ELSE
		ls_planta  =  is_NomPlanta
	END IF
END IF	
		
IF cbx_consolprod.CheCked THEN
	ls_Productor	=  "Consolidado"
ELSE	
   IF cbx_productor.CheCked  THEN
		ls_productor	=  "Todos"
	ELSE
		ls_productor  =  is_NomProductor
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

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_existencia_estadía_dias"
li_Orden = 1
	
vinf.dw_1.SetTransObject(sqlca)

Fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[5]), &
									 Long(istr_mant.argumento[4]),uo_selespecie.Codigo,uo_selvariedad.Codigo,&
									 li_Cond,li_Orden,Integer(istr_mant.argumento[27]),&
									 Integer(istr_mant.argumento[28]),ii_control,&
									 Integer(istr_mant.argumento[35]),li_varirotula)

IF Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify('planta.text = "' + ls_Planta + '"')
	vinf.dw_1.Modify("status.text = '" + istr_mant.argumento[36] + "'")
	vinf.dw_1.Modify('ccalidad.text = "' + is_control_d + '"')
	vinf.dw_1.Modify('productor.text = "' + ls_Productor + '"')
	vinf.dw_1.Modify('condicion.text = "' + ls_Condicion + '"')
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_estadia_dias
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2345
integer y = 1820
integer taborder = 130
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_cliente from datawindow within w_info_existencia_estadia_dias
integer x = 759
integer y = 452
integer width = 1230
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	data

IF NoExisteCliente(istr_mant.argumento[1]) THEN
	dw_cliente.SetItem(1, "clie_codigo", gi_codexport)
	dw_cliente.SetFocus()
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_existencia_estadia_dias
integer x = 343
integer y = 484
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

type st_4 from statictext within w_info_existencia_estadia_dias
integer x = 247
integer y = 440
integer width = 1966
integer height = 328
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

type st_especie from statictext within w_info_existencia_estadia_dias
integer x = 343
integer y = 796
integer width = 238
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
boolean focusrectangle = false
end type

type st_nro2 from statictext within w_info_existencia_estadia_dias
integer x = 247
integer y = 764
integer width = 1966
integer height = 392
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

type st_variedad from statictext within w_info_existencia_estadia_dias
integer x = 343
integer y = 1004
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

type st_productor from statictext within w_info_existencia_estadia_dias
integer x = 343
integer y = 1280
integer width = 297
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
string text = "Productor"
boolean focusrectangle = false
end type

type em_productor from editmask within w_info_existencia_estadia_dias
integer x = 759
integer y = 1264
integer width = 270
integer height = 96
integer taborder = 100
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

event modified;Long			ll_productor
Integer		li_cliente

IF This.Text <> '' AND This.Text <> '0' THEN
	ll_productor	=	Long(This.Text)
	li_cliente 		=  dw_cliente.Object.clie_codigo[1]
	
	SELECT	prod_nombre
		INTO	:is_NomProductor
		FROM	dbo.productores as pro,dbo.productoresclientes as cli
		WHERE	pro.prod_codigo =	:ll_productor
		AND	pro.prod_codigo = cli.prod_codigo
		AND	:li_cliente in (-1,cli.clie_codigo);
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla Productores")
		This.SetFocus()
	ELSEIF sqlca.SQLCode = 100 THEN
	
		MessageBox("Atención", "Código de productor no ha sido definido o no existe para este cliente.~r~r" + &
			"Ingrese o seleccione otro Código.")
		em_productor.Text 	= ''
		sle_productor.Text 	= ''
		This.SetFocus()
	ELSE
		sle_productor.Text		=	is_NomProductor
		istr_mant.argumento[4]	=	String(ll_productor)
	END IF
END IF	
	
end event

type cb_buscaproductor from commandbutton within w_info_existencia_estadia_dias
integer x = 1042
integer y = 1272
integer width = 96
integer height = 84
integer taborder = 110
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
	istr_mant.argumento[4]	=	lstr_busq.argum[3]
END IF
end event

type sle_productor from singlelineedit within w_info_existencia_estadia_dias
integer x = 1157
integer y = 1264
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

type cbx_productor from checkbox within w_info_existencia_estadia_dias
integer x = 759
integer y = 1176
integer width = 402
integer height = 80
integer taborder = 90
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
boolean checked = true
end type

event clicked;istr_mant.argumento[27] = "0"
IF cbx_productor.Checked THEN
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
	istr_mant.argumento[4]		=	'0'
	cbx_consolprod.Checked		=  False
ELSE
	em_productor.Enabled			=	True
	cb_buscaproductor.Enabled	=	True
END IF
end event

type st_planta from statictext within w_info_existencia_estadia_dias
integer x = 343
integer y = 684
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
string text = "Planta"
boolean focusrectangle = false
end type

type cbx_planta from checkbox within w_info_existencia_estadia_dias
integer x = 759
integer y = 568
integer width = 402
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
string text = "Todos"
boolean checked = true
end type

event clicked;call super::clicked;
istr_mant.argumento[28] = "0"
IF cbx_planta.Checked THEN
	dw_planta.Enabled			=	False
	istr_mant.argumento[5]	=	'0'
	is_NomPlanta				=	"TODAS"
	cbx_consolplan.Checked  =  False
ELSE
	dw_planta.Enabled			=	True
	istr_mant.argumento[5]	=	String(dw_planta.GetItemNumber(1,"plde_codigo"))
	is_NomPlanta				=	idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")
	
END IF
end event

type dw_planta from datawindow within w_info_existencia_estadia_dias
integer x = 759
integer y = 652
integer width = 969
integer height = 96
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[5]	=	data

IF NoExistePlanta(istr_mant.argumento[5]) THEN
	dw_planta.SetItem(1, "plde_codigo", gi_codplanta)
	is_NomPlanta				=	idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")
	dw_planta.SetFocus()
	RETURN 1
END IF




end event

event itemerror;RETURN 1
end event

type gb_5 from groupbox within w_info_existencia_estadia_dias
boolean visible = false
integer x = 2427
integer y = 720
integer width = 1792
integer height = 168
integer taborder = 150
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Orden"
end type

type st_5 from statictext within w_info_existencia_estadia_dias
integer x = 247
integer y = 1156
integer width = 1966
integer height = 568
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

type rb_embalaje from radiobutton within w_info_existencia_estadia_dias
boolean visible = false
integer x = 2546
integer y = 764
integer width = 485
integer height = 100
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Por Embalaje"
boolean checked = true
end type

type rb_productor from radiobutton within w_info_existencia_estadia_dias
boolean visible = false
integer x = 3086
integer y = 784
integer width = 485
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Por Productor"
end type

type rb_variedad from radiobutton within w_info_existencia_estadia_dias
boolean visible = false
integer x = 3451
integer y = 764
integer width = 485
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Por Variedad"
end type

type st_7 from statictext within w_info_existencia_estadia_dias
integer x = 247
integer y = 1724
integer width = 1966
integer height = 116
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

type cbx_consolplan from checkbox within w_info_existencia_estadia_dias
integer x = 1202
integer y = 568
integer width = 443
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
string text = "Consolidado"
end type

event clicked;istr_mant.argumento[28] = "0"
IF cbx_consolplan.Checked THEN
	dw_planta.Enabled			=	False
	istr_mant.argumento[5]	=	'0'
	is_NomPlanta				=	"TODAS"
	cbx_planta.Checked      =  False
	istr_mant.argumento[28] = "1"
ELSE
	dw_planta.Enabled			=	True
	istr_mant.argumento[5]	=	String(dw_planta.GetItemNumber(1,"plde_codigo"))
	is_NomPlanta				=	idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")
END IF
end event

type cbx_consolprod from checkbox within w_info_existencia_estadia_dias
integer x = 1143
integer y = 1176
integer width = 443
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
string text = "Consolidado"
end type

event clicked;istr_mant.argumento[27] = "0"
IF cbx_consolprod.Checked THEN
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
	istr_mant.argumento[4]		=	'0'
	cbx_productor.Checked		=  False
	istr_mant.argumento[27] = "1"
ELSE
	em_productor.Enabled			=	True
	cb_buscaproductor.Enabled	=	True
END IF
end event

type rb_controltodos from radiobutton within w_info_existencia_estadia_dias
integer x = 325
integer y = 1920
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked=True THEN
	ii_control	=	-1
	is_control_d= this.text
END IF
end event

type rb_rechazados from radiobutton within w_info_existencia_estadia_dias
integer x = 704
integer y = 1920
integer width = 434
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
string text = "Rechazados"
end type

event clicked;IF This.checked=True THEN
	ii_control	=	3
	is_control_d= this.text
END IF
end event

type rb_objetados from radiobutton within w_info_existencia_estadia_dias
integer x = 1234
integer y = 1920
integer width = 407
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
string text = "Objetados"
end type

event clicked;IF This.checked=True THEN
	ii_control	=	2
	is_control_d= this.text
END IF
end event

type rb_habilitado from radiobutton within w_info_existencia_estadia_dias
integer x = 1742
integer y = 1920
integer width = 407
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
string text = "Habilitados"
end type

event clicked;IF This.checked=True THEN
	ii_control	=	1
	is_control_d= this.text
END IF
end event

type gb_6 from groupbox within w_info_existencia_estadia_dias
integer x = 302
integer y = 1844
integer width = 1874
integer height = 180
integer taborder = 140
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Control Calidad"
end type

type st_1 from statictext within w_info_existencia_estadia_dias
integer x = 247
integer y = 1840
integer width = 1966
integer height = 216
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

type st_2 from statictext within w_info_existencia_estadia_dias
integer x = 347
integer y = 1456
integer width = 306
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
string text = "Condición"
boolean focusrectangle = false
end type

type uo_selcondicion from uo_seleccion_condicion within w_info_existencia_estadia_dias
integer x = 754
integer y = 1368
integer taborder = 120
boolean bringtotop = true
end type

on uo_selcondicion.destroy
call uo_seleccion_condicion::destroy
end on

type cbx_predio from checkbox within w_info_existencia_estadia_dias
boolean visible = false
integer x = 2501
integer y = 1060
integer width = 343
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Predio"
end type

type st_3 from statictext within w_info_existencia_estadia_dias
integer x = 347
integer y = 1612
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

type dw_stat from datawindow within w_info_existencia_estadia_dias
boolean visible = false
integer x = 731
integer y = 1576
integer width = 969
integer height = 88
integer taborder = 120
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[6]	=	data
istr_mant.argumento[7]	=  f_statnombre(integer(data))

end event

type cbx_status from checkbox within w_info_existencia_estadia_dias
integer x = 754
integer y = 1596
integer width = 498
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
string text = "Consolidados    "
end type

event clicked;IF This.Checked THEN
   istr_mant.argumento[35]   =	'1'							// status
   istr_mant.argumento[36]	=  "Consolidados "			// status nombre
ELSE
   istr_mant.argumento[35]   =	'0'							// status
   istr_mant.argumento[36]	=  "Todos "						// status nombre
END IF	
//IF This.Checked THEN
//	dw_stat.Enabled			=	False
//	istr_mant.argumento[6]   =	'0'
//	istr_mant.argumento[7]	=  "Todos"	
//	dw_stat.Reset()
//	dw_stat.InsertRow(0)
//	dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(166,180,210)
//	
//ELSE
//	dw_stat.Enabled			=	True	
//	dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(255, 255, 255)
//	dw_stat.InsertRow(0)
//	dw_stat.SetItem(1, "stat_codigo", 1)
//	istr_mant.argumento[7]	=  "Normal"
//END IF
end event

type uo_selespecie from uo_seleccion_especie within w_info_existencia_estadia_dias
integer x = 759
integer y = 776
integer height = 180
integer taborder = 240
boolean bringtotop = true
end type

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

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_estadia_dias
integer x = 759
integer y = 964
integer taborder = 260
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_existencia_estadia_dias
integer x = 937
integer y = 1744
integer width = 576
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
string text = "Variedad Rotulada"
end type

