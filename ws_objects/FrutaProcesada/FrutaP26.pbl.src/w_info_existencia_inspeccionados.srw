$PBExportHeader$w_info_existencia_inspeccionados.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_existencia_inspeccionados from w_para_informes
end type
type gb_3 from groupbox within w_info_existencia_inspeccionados
end type
type st_1 from statictext within w_info_existencia_inspeccionados
end type
type dw_cliente from datawindow within w_info_existencia_inspeccionados
end type
type st_2 from statictext within w_info_existencia_inspeccionados
end type
type dw_especie from datawindow within w_info_existencia_inspeccionados
end type
type st_3 from statictext within w_info_existencia_inspeccionados
end type
type em_fzarpe from editmask within w_info_existencia_inspeccionados
end type
type st_6 from statictext within w_info_existencia_inspeccionados
end type
type st_8 from statictext within w_info_existencia_inspeccionados
end type
type dw_planta from datawindow within w_info_existencia_inspeccionados
end type
type rb_aprobados from radiobutton within w_info_existencia_inspeccionados
end type
type rb_despachados from radiobutton within w_info_existencia_inspeccionados
end type
type cbx_todos from checkbox within w_info_existencia_inspeccionados
end type
type rb_anulados from radiobutton within w_info_existencia_inspeccionados
end type
type st_17 from statictext within w_info_existencia_inspeccionados
end type
type st_13 from statictext within w_info_existencia_inspeccionados
end type
type st_7 from statictext within w_info_existencia_inspeccionados
end type
type em_hasta from editmask within w_info_existencia_inspeccionados
end type
type st_14 from statictext within w_info_existencia_inspeccionados
end type
type st_4 from statictext within w_info_existencia_inspeccionados
end type
type cbx_conscliente from checkbox within w_info_existencia_inspeccionados
end type
type cbx_2 from checkbox within w_info_existencia_inspeccionados
end type
type st_5 from statictext within w_info_existencia_inspeccionados
end type
type cbx_unisag from checkbox within w_info_existencia_inspeccionados
end type
type st_9 from statictext within w_info_existencia_inspeccionados
end type
type em_fechaexi from editmask within w_info_existencia_inspeccionados
end type
type st_10 from statictext within w_info_existencia_inspeccionados
end type
type cbx_nuevo from checkbox within w_info_existencia_inspeccionados
end type
end forward

global type w_info_existencia_inspeccionados from w_para_informes
integer width = 3049
integer height = 1884
boolean center = true
gb_3 gb_3
st_1 st_1
dw_cliente dw_cliente
st_2 st_2
dw_especie dw_especie
st_3 st_3
em_fzarpe em_fzarpe
st_6 st_6
st_8 st_8
dw_planta dw_planta
rb_aprobados rb_aprobados
rb_despachados rb_despachados
cbx_todos cbx_todos
rb_anulados rb_anulados
st_17 st_17
st_13 st_13
st_7 st_7
em_hasta em_hasta
st_14 st_14
st_4 st_4
cbx_conscliente cbx_conscliente
cbx_2 cbx_2
st_5 st_5
cbx_unisag cbx_unisag
st_9 st_9
em_fechaexi em_fechaexi
st_10 st_10
cbx_nuevo cbx_nuevo
end type
global w_info_existencia_inspeccionados w_info_existencia_inspeccionados

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_especie, idwc_embarque,idwc_operaciones,&
                     idwc_planta, idwc_productor

Integer	ii_Cliente, ii_Especie, ii_Planta, ii_filtro
String	is_Embarque, is_NomEspecie, is_NomEmbarque, is_NomNave, is_NomPlanta, &
			is_NomCliente, is_OPeracion, is_NomRecibidor, is_NomFiltro
Long		ii_productor, il_codsag
Date		id_FechaZarpe


end variables

forward prototypes
public function boolean existeproductor (long productor)
public function boolean noexisteplanta (integer ai_cliente, integer ai_planta)
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

public function boolean noexisteplanta (integer ai_cliente, integer ai_planta);Integer	li_Zona, li_TipoPlanta, li_CodigoSag

SELECT	plde_nombre, plde_codsag
	INTO	:is_NomPlanta,:il_codsag
	FROM	dbo.PLANTADESP
	WHERE	plde_codigo	=	:ai_Planta ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Plantas")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro código.")
	
	RETURN False
ELSE
	
END IF	

RETURN TRUE
end function

on w_info_existencia_inspeccionados.create
int iCurrent
call super::create
this.gb_3=create gb_3
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.dw_especie=create dw_especie
this.st_3=create st_3
this.em_fzarpe=create em_fzarpe
this.st_6=create st_6
this.st_8=create st_8
this.dw_planta=create dw_planta
this.rb_aprobados=create rb_aprobados
this.rb_despachados=create rb_despachados
this.cbx_todos=create cbx_todos
this.rb_anulados=create rb_anulados
this.st_17=create st_17
this.st_13=create st_13
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_14=create st_14
this.st_4=create st_4
this.cbx_conscliente=create cbx_conscliente
this.cbx_2=create cbx_2
this.st_5=create st_5
this.cbx_unisag=create cbx_unisag
this.st_9=create st_9
this.em_fechaexi=create em_fechaexi
this.st_10=create st_10
this.cbx_nuevo=create cbx_nuevo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_cliente
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.dw_especie
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.em_fzarpe
this.Control[iCurrent+8]=this.st_6
this.Control[iCurrent+9]=this.st_8
this.Control[iCurrent+10]=this.dw_planta
this.Control[iCurrent+11]=this.rb_aprobados
this.Control[iCurrent+12]=this.rb_despachados
this.Control[iCurrent+13]=this.cbx_todos
this.Control[iCurrent+14]=this.rb_anulados
this.Control[iCurrent+15]=this.st_17
this.Control[iCurrent+16]=this.st_13
this.Control[iCurrent+17]=this.st_7
this.Control[iCurrent+18]=this.em_hasta
this.Control[iCurrent+19]=this.st_14
this.Control[iCurrent+20]=this.st_4
this.Control[iCurrent+21]=this.cbx_conscliente
this.Control[iCurrent+22]=this.cbx_2
this.Control[iCurrent+23]=this.st_5
this.Control[iCurrent+24]=this.cbx_unisag
this.Control[iCurrent+25]=this.st_9
this.Control[iCurrent+26]=this.em_fechaexi
this.Control[iCurrent+27]=this.st_10
this.Control[iCurrent+28]=this.cbx_nuevo
end on

on w_info_existencia_inspeccionados.destroy
call super::destroy
destroy(this.gb_3)
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.dw_especie)
destroy(this.st_3)
destroy(this.em_fzarpe)
destroy(this.st_6)
destroy(this.st_8)
destroy(this.dw_planta)
destroy(this.rb_aprobados)
destroy(this.rb_despachados)
destroy(this.cbx_todos)
destroy(this.rb_anulados)
destroy(this.st_17)
destroy(this.st_13)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_14)
destroy(this.st_4)
destroy(this.cbx_conscliente)
destroy(this.cbx_2)
destroy(this.st_5)
destroy(this.cbx_unisag)
destroy(this.st_9)
destroy(this.em_fechaexi)
destroy(this.st_10)
destroy(this.cbx_nuevo)
end on

event open;call super::open;String	ls_Columna[], ls_operacion="TODAS",ls_Planta
Integer 	li_busca, li_region

SELECT	plde_nombre, plde_region
	INTO	:ls_Planta,:li_region
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:gi_CodPlanta ;

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.Object.clie_codigo[1]	=	gi_CodExport
li_busca = idwc_cliente.Find("clie_codigo = " + String(gi_CodExport), 1, idwc_cliente.RowCount())
is_NomCliente = idwc_cliente.GetItemString(li_busca, "clie_nombre")

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_especie.Object.espe_codigo[1]	=	gi_CodEspecie

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.Object.plde_codigo[1] = gi_CodPlanta
li_busca = idwc_planta.Find("plde_codigo = " + String(gi_CodPlanta), 1, idwc_planta.RowCount())
is_NomPlanta = idwc_planta.GetItemString(li_busca, "plde_nombre")

NoExistePlanta(gi_CodExport,gi_CodPlanta)

ExisteEspecie(gi_CodExport, gi_CodEspecie, ls_Columna[])

ii_Cliente						=	gi_CodExport
ii_Especie						=	gi_CodEspecie
ii_Planta						=	gi_CodPlanta
istr_mant.argumento[1]		= 	String(gi_codexport)		//	Cliente
istr_mant.argumento[2]		= 	String(gi_codespecie)	//	Especie
istr_mant.argumento[3]		=	'0'							// Variedad
istr_mant.argumento[5]		= 	String(gi_CodPlanta)		//	Planta
istr_mant.argumento[11]		=	String(li_region)

is_NomEspecie					=	ls_Columna[1]

em_fzarpe.text					=	String(RelativeDate(Today() , -365))
em_hasta.text					=	String(Today())
em_fechaexi.text				=	String(Today())

cbx_todos.checked				=	True
ii_filtro 						= 	-1
is_NomFiltro 					= 	"Todos"
rb_anulados.Checked			=	False
rb_despachados.Checked		=	False
rb_aprobados.Checked			=	False
rb_anulados.Enabled			=	False
rb_despachados.Enabled		=	False	
rb_aprobados.enabled  		=  False
end event

type pb_excel from w_para_informes`pb_excel within w_info_existencia_inspeccionados
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_inspeccionados
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_inspeccionados
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_inspeccionados
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_inspeccionados
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_inspeccionados
integer width = 2130
integer height = 80
string text = "Informe Existencia De Productos Inspeccionados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_inspeccionados
integer x = 2551
integer y = 948
integer taborder = 60
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Long	  		ll_Fila, li_Busca
Integer   	li_Cliente, li_region
String		t_fecha, is_Instructivo, ls_region, ls_especie
Date 			ld_fechaZarpe

SetPointer(HourGlass!)

li_region	=	Integer(istr_mant.argumento[11])

IF li_region = 1 THEN ls_region 	= 'I Región'
IF li_region = 2 THEN ls_region 	= 'II Región'
IF li_region = 3 THEN ls_region 	= 'III Región'
IF li_region = 4 THEN ls_region 	= 'IV Región'
IF li_region = 5 THEN ls_region 	= 'V Región'
IF li_region = 6 THEN ls_region 	= 'VI Región'
IF li_region = 7 THEN ls_region 	= 'VII Región'
IF li_region = 8 THEN ls_region 	= 'VIII Región'
IF li_region = 9 THEN ls_region 	= 'IX Región'
IF li_region = 10 THEN ls_region = 'X Región'
IF li_region = 11 THEN ls_region = 'XI Región'
IF li_region = 12 THEN ls_region = 'XII Región'
IF li_region = 13 THEN ls_region = 'Región Metropolitana'

IF cbx_conscliente.Checked THEN
	is_NomCliente	=	'Consolidado'
ELSE
	li_Cliente	= dw_cliente.Object.clie_codigo[1]
	li_busca = idwc_cliente.Find("clie_codigo = " + String(li_Cliente), 1, idwc_cliente.RowCount())
	is_NomCliente = idwc_cliente.GetItemString(li_busca, "clie_nombre")
END IF

ls_Especie	=	dw_especie.Object.espe_nombre[1]

istr_info.titulo	= 'INFORME EXISTENCIA DE PRODUCTOS INSPECCIONADOS.'

OpenWithParm(vinf, istr_info)

IF cbx_nuevo.Checked THEN
	vinf.dw_1.DataObject = "dw_info_existenciainspeccionados_unisag_nuevo"
ELSE
	IF cbx_unisag.Checked THEN
		vinf.dw_1.DataObject = "dw_info_existenciainspeccionados_unisag_ult"//"dw_info_existenciainspeccionados_unisag"
	ELSE
		vinf.dw_1.DataObject = "dw_info_existenciainspeccionados"	
	END IF
END IF	
vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente, ii_Especie, ii_Planta, Date(em_fzarpe.text),Date(em_hasta.text), ii_filtro)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("planta.text = '" + is_NomPlanta + "'")
	vinf.dw_1.Modify("especie.text = '" + is_NomEspecie + "'")
	vinf.dw_1.Modify("filtro.text = '" + is_NomFiltro + "'")
	vinf.dw_1.Modify("cliente.text = '" + is_NomCliente + "'")
	vinf.dw_1.Modify("region.text = '" + ls_region + "'")		
	vinf.dw_1.Modify("fechastart.text = '" + em_fzarpe.text + "'")
	vinf.dw_1.Modify("fechaend.text = '" + em_hasta.text + "'")
	vinf.dw_1.Modify("fechaexi.text = '" + em_fechaexi.text + "'")
	vinf.dw_1.Modify("codsag.text = '" + String(il_codsag) + "'")
	
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF


end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_inspeccionados
integer x = 2551
integer y = 1288
integer taborder = 70
end type

type gb_3 from groupbox within w_info_existencia_inspeccionados
integer x = 288
integer y = 1040
integer width = 2030
integer height = 256
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Filtro"
end type

type st_1 from statictext within w_info_existencia_inspeccionados
integer x = 247
integer y = 440
integer width = 2126
integer height = 144
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_existencia_inspeccionados
integer x = 727
integer y = 472
integer width = 1147
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

	dw_especie.GetChild("espe_codigo", idwc_especie)
	idwc_especie.SetTransObject(SQLCA)
	idwc_especie.Retrieve(Integer(istr_mant.argumento[1]))
	istr_mant.argumento[2]	=	String(dw_especie.Object.espe_codigo[1])
	ii_especie	=	integer(istr_mant.argumento[2])


	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(SQLCA)
	idwc_planta.Retrieve(1)
	istr_mant.argumento[3]	=	String(dw_planta.Object.plde_codigo[1])
	ii_planta	=	integer(istr_mant.argumento[3])

ELSE
	This.SetItem(1, "clie_codigo", li_nula)
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_existencia_inspeccionados
integer x = 302
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

type dw_especie from datawindow within w_info_existencia_inspeccionados
integer x = 727
integer y = 756
integer width = 878
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer li_Nula

IsNull(li_Nula)
IF ExisteEspecie(ii_Cliente, Integer(data), ls_Columna[]) THEN
	ii_Especie		=	Integer(data)
	is_NomEspecie	=	ls_Columna[1]
  
ELSE
	This.SetItem(1, "espe_codigo", li_Nula)
	RETURN 1
END IF
end event

type st_3 from statictext within w_info_existencia_inspeccionados
integer x = 302
integer y = 768
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

type em_fzarpe from editmask within w_info_existencia_inspeccionados
integer x = 727
integer y = 900
integer width = 402
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event em_fzarpe::modified;call super::modified;//istr_mant.argumento[7]=this.text
end event

type st_6 from statictext within w_info_existencia_inspeccionados
integer x = 302
integer y = 912
integer width = 411
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
string text = "Fecha  Desde"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_existencia_inspeccionados
integer x = 302
integer y = 624
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

type dw_planta from datawindow within w_info_existencia_inspeccionados
integer x = 727
integer y = 612
integer width = 969
integer height = 92
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF NoExistePlanta(ii_Cliente,Integer(data)) THEN
	ii_Planta		=	Integer(data)
	
ELSE
	This.SetItem(1, "plde_codigo", li_Nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type rb_aprobados from radiobutton within w_info_existencia_inspeccionados
integer x = 457
integer y = 1184
integer width = 485
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
boolean enabled = false
string text = "Aprobados"
end type

event clicked;ii_filtro = 1
is_NomFiltro = this.text
end event

type rb_despachados from radiobutton within w_info_existencia_inspeccionados
integer x = 1029
integer y = 1184
integer width = 498
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
boolean enabled = false
string text = "Despachados"
end type

event clicked;ii_filtro = 2
is_NomFiltro = this.text
end event

type cbx_todos from checkbox within w_info_existencia_inspeccionados
integer x = 1038
integer y = 1096
integer width = 539
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
	ii_filtro = -1
	is_NomFiltro = this.text
	istr_mant.argumento[9]		=	'0'
	rb_anulados.Checked			=	False
	rb_despachados.Checked		=	False
	rb_aprobados.Checked			=	False
	rb_anulados.Enabled			=	False
	rb_despachados.Enabled		=	False	
	rb_aprobados.enabled  		=  False
ELSE
	SetNull(ii_filtro)
	istr_mant.argumento[9]		=	'0'
	rb_anulados.Enabled			=	True
	rb_despachados.Enabled		=	True	
	rb_aprobados.enabled  		=  True
END IF
end event

type rb_anulados from radiobutton within w_info_existencia_inspeccionados
integer x = 1600
integer y = 1184
integer width = 498
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
boolean enabled = false
string text = "Anulados"
end type

event clicked;ii_filtro = 3
is_NomFiltro = this.text
end event

type st_17 from statictext within w_info_existencia_inspeccionados
integer x = 247
integer y = 1020
integer width = 2126
integer height = 304
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_existencia_inspeccionados
integer x = 247
integer y = 872
integer width = 2126
integer height = 144
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_existencia_inspeccionados
integer x = 247
integer y = 728
integer width = 2126
integer height = 144
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_existencia_inspeccionados
integer x = 1481
integer y = 900
integer width = 402
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_14 from statictext within w_info_existencia_inspeccionados
integer x = 1216
integer y = 912
integer width = 206
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

type st_4 from statictext within w_info_existencia_inspeccionados
integer x = 247
integer y = 584
integer width = 2126
integer height = 144
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_conscliente from checkbox within w_info_existencia_inspeccionados
integer x = 1911
integer y = 480
integer width = 457
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
end type

event clicked;IF This.Checked THEN
	ii_Cliente										=	-9
	dw_cliente.Object.clie_codigo.Protect	=	1
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	ii_Cliente										=	gi_CodExport
	dw_cliente.Object.clie_codigo.Protect	=	0
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_cliente.SetFocus()
END IF
end event

type cbx_2 from checkbox within w_info_existencia_inspeccionados
boolean visible = false
integer x = 1902
integer y = 764
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
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	ii_Especie						=	-1
	dw_especie.Object.espe_codigo.Protect	=	1
	dw_especie.Object.espe_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	ii_Especie						=	gi_CodEspecie
	dw_especie.Object.espe_codigo.Protect	=	0
	dw_especie.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_especie.SetFocus()
END IF
end event

type st_5 from statictext within w_info_existencia_inspeccionados
integer x = 247
integer y = 1324
integer width = 2126
integer height = 132
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_unisag from checkbox within w_info_existencia_inspeccionados
integer x = 887
integer y = 1356
integer width = 709
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
string text = "Formato Único S.A.G."
boolean checked = true
end type

type st_9 from statictext within w_info_existencia_inspeccionados
integer x = 302
integer y = 1500
integer width = 457
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
string text = "Fecha Emisión "
boolean focusrectangle = false
end type

type em_fechaexi from editmask within w_info_existencia_inspeccionados
integer x = 782
integer y = 1488
integer width = 402
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_10 from statictext within w_info_existencia_inspeccionados
integer x = 247
integer y = 1460
integer width = 2126
integer height = 144
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_nuevo from checkbox within w_info_existencia_inspeccionados
integer x = 1294
integer y = 1496
integer width = 983
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
string text = "Formato Único S.A.G. Nuevo"
end type

event clicked;IF This.Checked THEN
	cbx_unisag.Enabled = False
	cbx_unisag.Checked = False
ELSE	
	cbx_unisag.Enabled = True
	cbx_unisag.Enabled = True
END IF	
end event

