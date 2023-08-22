$PBExportHeader$w_info_reetiquetado_formatoexcel.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_reetiquetado_formatoexcel from w_para_informes
end type
type dw_cliente from datawindow within w_info_reetiquetado_formatoexcel
end type
type st_2 from statictext within w_info_reetiquetado_formatoexcel
end type
type st_3 from statictext within w_info_reetiquetado_formatoexcel
end type
type em_fzarpe from editmask within w_info_reetiquetado_formatoexcel
end type
type st_8 from statictext within w_info_reetiquetado_formatoexcel
end type
type dw_planta from datawindow within w_info_reetiquetado_formatoexcel
end type
type em_hasta from editmask within w_info_reetiquetado_formatoexcel
end type
type st_14 from statictext within w_info_reetiquetado_formatoexcel
end type
type st_5 from statictext within w_info_reetiquetado_formatoexcel
end type
type st_9 from statictext within w_info_reetiquetado_formatoexcel
end type
type st_4 from statictext within w_info_reetiquetado_formatoexcel
end type
type st_7 from statictext within w_info_reetiquetado_formatoexcel
end type
type dw_tipop from datawindow within w_info_reetiquetado_formatoexcel
end type
type dw_productor from datawindow within w_info_reetiquetado_formatoexcel
end type
type cbx_todostipop from checkbox within w_info_reetiquetado_formatoexcel
end type
type cbx_todosprod from checkbox within w_info_reetiquetado_formatoexcel
end type
type em_numdesde from editmask within w_info_reetiquetado_formatoexcel
end type
type em_numhasta from editmask within w_info_reetiquetado_formatoexcel
end type
type gb_3 from groupbox within w_info_reetiquetado_formatoexcel
end type
type gb_4 from groupbox within w_info_reetiquetado_formatoexcel
end type
type st_12 from statictext within w_info_reetiquetado_formatoexcel
end type
type st_15 from statictext within w_info_reetiquetado_formatoexcel
end type
type dw_embalaje from datawindow within w_info_reetiquetado_formatoexcel
end type
type cbx_todosembalaje from checkbox within w_info_reetiquetado_formatoexcel
end type
type cbx_cliente from checkbox within w_info_reetiquetado_formatoexcel
end type
type cbx_planta from checkbox within w_info_reetiquetado_formatoexcel
end type
type st_1 from statictext within w_info_reetiquetado_formatoexcel
end type
type cbx_1 from checkbox within w_info_reetiquetado_formatoexcel
end type
type uo_selespecie from uo_seleccion_especie within w_info_reetiquetado_formatoexcel
end type
type cbx_varirotula from checkbox within w_info_reetiquetado_formatoexcel
end type
end forward

global type w_info_reetiquetado_formatoexcel from w_para_informes
integer width = 2953
integer height = 2024
string title = "REETIQUETADO HISTORICO"
boolean maxbox = false
boolean resizable = false
dw_cliente dw_cliente
st_2 st_2
st_3 st_3
em_fzarpe em_fzarpe
st_8 st_8
dw_planta dw_planta
em_hasta em_hasta
st_14 st_14
st_5 st_5
st_9 st_9
st_4 st_4
st_7 st_7
dw_tipop dw_tipop
dw_productor dw_productor
cbx_todostipop cbx_todostipop
cbx_todosprod cbx_todosprod
em_numdesde em_numdesde
em_numhasta em_numhasta
gb_3 gb_3
gb_4 gb_4
st_12 st_12
st_15 st_15
dw_embalaje dw_embalaje
cbx_todosembalaje cbx_todosembalaje
cbx_cliente cbx_cliente
cbx_planta cbx_planta
st_1 st_1
cbx_1 cbx_1
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
end type
global w_info_reetiquetado_formatoexcel w_info_reetiquetado_formatoexcel

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_planta,idwc_tipop,&
                     idwc_productor, idwc_destino,idwc_pesoneto,idwc_embalaje,&
							idwc_mercados,idwc_condicion

Integer	ii_Cliente, ii_Planta, ii_filtro,ii_tipo
String	is_NomPlanta, is_NomCliente, is_embalajes
Long		il_productor
Date		id_FechaZarpe

uo_productores     		iuo_productores   
uo_tipoproductor   		iuo_tipoproductor
uo_destinos        		iuo_destinos
uo_embalajesprod   		iuo_embalajesprod
uo_seleccion_especie		iuo_selespecie


end variables

forward prototypes
public function boolean existeproductor (long productor)
end prototypes

public function boolean existeproductor (long productor);String	ls_Nombre
Integer	li_cliente

IF cbx_cliente.Checked THEN
	li_cliente	= -1
ELSE
	li_cliente	= dw_cliente.Object.clie_Codigo[1]
END IF	

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores as pro,dbo.productoresclientes as cli
	WHERE	pro.prod_codigo =	:productor
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
	il_productor = Productor
	RETURN True
END IF
end function

on w_info_reetiquetado_formatoexcel.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.st_3=create st_3
this.em_fzarpe=create em_fzarpe
this.st_8=create st_8
this.dw_planta=create dw_planta
this.em_hasta=create em_hasta
this.st_14=create st_14
this.st_5=create st_5
this.st_9=create st_9
this.st_4=create st_4
this.st_7=create st_7
this.dw_tipop=create dw_tipop
this.dw_productor=create dw_productor
this.cbx_todostipop=create cbx_todostipop
this.cbx_todosprod=create cbx_todosprod
this.em_numdesde=create em_numdesde
this.em_numhasta=create em_numhasta
this.gb_3=create gb_3
this.gb_4=create gb_4
this.st_12=create st_12
this.st_15=create st_15
this.dw_embalaje=create dw_embalaje
this.cbx_todosembalaje=create cbx_todosembalaje
this.cbx_cliente=create cbx_cliente
this.cbx_planta=create cbx_planta
this.st_1=create st_1
this.cbx_1=create cbx_1
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.em_fzarpe
this.Control[iCurrent+5]=this.st_8
this.Control[iCurrent+6]=this.dw_planta
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.st_14
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_9
this.Control[iCurrent+11]=this.st_4
this.Control[iCurrent+12]=this.st_7
this.Control[iCurrent+13]=this.dw_tipop
this.Control[iCurrent+14]=this.dw_productor
this.Control[iCurrent+15]=this.cbx_todostipop
this.Control[iCurrent+16]=this.cbx_todosprod
this.Control[iCurrent+17]=this.em_numdesde
this.Control[iCurrent+18]=this.em_numhasta
this.Control[iCurrent+19]=this.gb_3
this.Control[iCurrent+20]=this.gb_4
this.Control[iCurrent+21]=this.st_12
this.Control[iCurrent+22]=this.st_15
this.Control[iCurrent+23]=this.dw_embalaje
this.Control[iCurrent+24]=this.cbx_todosembalaje
this.Control[iCurrent+25]=this.cbx_cliente
this.Control[iCurrent+26]=this.cbx_planta
this.Control[iCurrent+27]=this.st_1
this.Control[iCurrent+28]=this.cbx_1
this.Control[iCurrent+29]=this.uo_selespecie
this.Control[iCurrent+30]=this.cbx_varirotula
end on

on w_info_reetiquetado_formatoexcel.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_fzarpe)
destroy(this.st_8)
destroy(this.dw_planta)
destroy(this.em_hasta)
destroy(this.st_14)
destroy(this.st_5)
destroy(this.st_9)
destroy(this.st_4)
destroy(this.st_7)
destroy(this.dw_tipop)
destroy(this.dw_productor)
destroy(this.cbx_todostipop)
destroy(this.cbx_todosprod)
destroy(this.em_numdesde)
destroy(this.em_numhasta)
destroy(this.gb_3)
destroy(this.gb_4)
destroy(this.st_12)
destroy(this.st_15)
destroy(this.dw_embalaje)
destroy(this.cbx_todosembalaje)
destroy(this.cbx_cliente)
destroy(this.cbx_planta)
destroy(this.st_1)
destroy(this.cbx_1)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
end on

event open;call super::open;x				= 0
y				= 0
//This.Height	= 2020

String	ls_Columna[], ls_operacion="TODAS"
Integer 	li_busca
Boolean	lb_Cerrar

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.Object.clie_codigo[1]	=	gi_CodExport
li_busca = idwc_cliente.Find("clie_codigo = " + String(gi_CodExport), 1, idwc_cliente.RowCount())
is_NomCliente = idwc_cliente.GetItemString(li_busca, "clie_nombre")

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.Object.plde_codigo[1] = gi_CodPlanta
li_busca = idwc_planta.Find("plde_codigo = " + String(gi_CodPlanta), 1, idwc_planta.RowCount())
is_NomPlanta = idwc_planta.GetItemString(li_busca, "plde_nombre")

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

dw_tipop.GetChild("tipr_codigo", idwc_tipop)
idwc_tipop.SetTransObject(SQLCA)
idwc_tipop.Retrieve()
dw_tipop.InsertRow(0)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(SQLCA)
idwc_productor.Retrieve(0,gi_CodExport)
dw_productor.InsertRow(0)

dw_embalaje.GetChild("emba_codigo", idwc_embalaje)
idwc_embalaje.SetTransObject(SQLCA)
idwc_embalaje.Retrieve(gi_CodExport)
dw_embalaje.InsertRow(0)

ii_Cliente						=	gi_CodExport
ii_Planta						=	gi_CodPlanta
istr_mant.argumento[1]		= 	String(gi_codexport)		//	Cliente
istr_mant.argumento[3]		= 	String(gi_CodPlanta)		//	Planta
em_fzarpe.text					=	String(RelativeDate(Today() , -365))
em_hasta.text					=	String(Today())	

cbx_todostipop.Checked = True
cbx_todosprod.Checked = True

dw_tipop.Object.tipr_codigo.Protect	=	1
dw_tipop.Object.tipr_codigo.Color	=	0
dw_tipop.Object.tipr_codigo.BackGround.Color	=	553648127
dw_productor.Object.prod_codigo.Protect	=	1
dw_productor.Object.prod_codigo.Color	=	0
dw_productor.Object.prod_codigo.BackGround.Color	=	553648127

dw_embalaje.Object.emba_codigo.Protect	=	1
dw_embalaje.Object.emba_codigo.Color	=	0
dw_embalaje.Object.emba_codigo.BackGround.Color	=	553648127

em_numdesde.Text = '1'
em_numhasta.Text = '99999999'
end event

type pb_excel from w_para_informes`pb_excel within w_info_reetiquetado_formatoexcel
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_computador from w_para_informes`st_computador within w_info_reetiquetado_formatoexcel
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_reetiquetado_formatoexcel
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_reetiquetado_formatoexcel
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_reetiquetado_formatoexcel
end type

type st_titulo from w_para_informes`st_titulo within w_info_reetiquetado_formatoexcel
integer width = 2171
integer height = 104
string text = "Informe Reetiquetado"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_reetiquetado_formatoexcel
integer x = 2551
integer y = 1132
integer taborder = 160
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

event pb_acepta::clicked;Long	  	ll_Fila
Integer	li_varirotula
String	t_fecha, is_Instructivo,ls_cajas
Date 		ld_fechaZarpe

SetPointer(HourGlass!)
istr_info.titulo	= 'INFORME CONDICIÓN HISTORICA'
OpenWithParm(vinf, istr_info)

/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

IF cbx_todosprod.Checked THEN
	il_productor = 0
ELSE
	il_productor = dw_productor.Object.prod_codigo[1]
END IF

IF cbx_todosembalaje.Checked THEN
	is_embalajes = '0'
ELSE
	is_embalajes = dw_embalaje.Object.emba_codigo[1]
END IF

vinf.dw_1.DataObject ="dw_info_reetiquetado_formatoexcel"
vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente, ii_Planta,uo_selespecie.Codigo,ii_tipo,il_productor,&
												  Date(em_fzarpe.text),Date(em_hasta.text), Long(em_numdesde.text),&
												  Long(em_numhasta.text),is_embalajes,li_varirotula)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("desde.text = '" + em_fzarpe.text + "'")
	vinf.dw_1.Modify("hasta.text = '" + em_hasta.text + "'")
		
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
end event

type pb_salir from w_para_informes`pb_salir within w_info_reetiquetado_formatoexcel
integer x = 2551
integer y = 1420
integer taborder = 170
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type dw_cliente from datawindow within w_info_reetiquetado_formatoexcel
integer x = 901
integer y = 596
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

	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(SQLCA)
	idwc_planta.Retrieve(1)
	istr_mant.argumento[3]	=	String(dw_planta.Object.plde_codigo[1])
	ii_planta	=	integer(istr_mant.argumento[3])
	
	IF cbx_todostipop.Checked THEN
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(0,dw_cliente.Object.clie_codigo[1])
		il_productor	=	dw_productor.Object.prod_codigo[1]
	ELSE
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(dw_tipop.Object.tipr_codigo[1],dw_cliente.Object.clie_codigo[1])
		il_productor	=	dw_productor.Object.prod_codigo[1]
	END IF	

ELSE
	This.SetItem(1, "clie_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_reetiquetado_formatoexcel
integer x = 347
integer y = 608
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

type st_3 from statictext within w_info_reetiquetado_formatoexcel
integer x = 347
integer y = 884
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

type em_fzarpe from editmask within w_info_reetiquetado_formatoexcel
integer x = 901
integer y = 1660
integer width = 402
integer height = 92
integer taborder = 130
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

type st_8 from statictext within w_info_reetiquetado_formatoexcel
integer x = 347
integer y = 712
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

type dw_planta from datawindow within w_info_reetiquetado_formatoexcel
integer x = 901
integer y = 700
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

type em_hasta from editmask within w_info_reetiquetado_formatoexcel
integer x = 1714
integer y = 1660
integer width = 402
integer height = 92
integer taborder = 140
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

type st_14 from statictext within w_info_reetiquetado_formatoexcel
integer x = 1472
integer y = 1684
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

type st_5 from statictext within w_info_reetiquetado_formatoexcel
integer x = 581
integer y = 1476
integer width = 256
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
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_reetiquetado_formatoexcel
integer x = 1472
integer y = 1476
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

type st_4 from statictext within w_info_reetiquetado_formatoexcel
integer x = 347
integer y = 996
integer width = 466
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
string text = "Tipo Productor"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_reetiquetado_formatoexcel
integer x = 347
integer y = 1100
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

type dw_tipop from datawindow within w_info_reetiquetado_formatoexcel
integer x = 901
integer y = 984
integer width = 905
integer height = 92
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_tipoproduc"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula
SetNull(li_nula)

iuo_tipoproductor = Create uo_tipoproductor

IF iuo_tipoproductor.existe(Integer(data),True,SqlCa) THEN
	ii_tipo	=	integer(data)
	
	IF cbx_cliente.Checked THEN
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(Integer(Data),-1)
		il_productor	=	dw_productor.Object.prod_codigo[1]
	ELSE
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(Integer(Data),dw_cliente.Object.clie_codigo[1])
		il_productor	=	dw_productor.Object.prod_codigo[1]
	END IF	
ELSE
	This.SetItem(1, "tipr_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type dw_productor from datawindow within w_info_reetiquetado_formatoexcel
integer x = 901
integer y = 1088
integer width = 1006
integer height = 92
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_productores_tipoprod_clientes"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF cbx_todosprod.Checked = TRUE THEN
	ii_tipo = 0
END IF	


iuo_productores = Create uo_productores

IF iuo_productores.existe_tipr(ii_tipo,Long(data),True,SqlCa) THEN
	il_productor	=	Long(data)
ELSE
	This.SetItem(1, "prod_codigo", li_Nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_todostipop from checkbox within w_info_reetiquetado_formatoexcel
integer x = 2062
integer y = 988
integer width = 311
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

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_tipo = 0
	dw_tipop.SetItem(1,"tipr_codigo",li_null)
	dw_tipop.Object.tipr_codigo.Protect	=	1
	dw_tipop.Object.tipr_codigo.BackGround.Color	=	RGB(166,180,210)
	
	IF cbx_cliente.Checked THEN
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(0,-1)
	END IF	
	
ELSE
	dw_tipop.Object.tipr_codigo.Protect	=	0
	dw_tipop.Object.tipr_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_tipop.SetFocus()
END IF
end event

type cbx_todosprod from checkbox within w_info_reetiquetado_formatoexcel
integer x = 2062
integer y = 1092
integer width = 297
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

event clicked;Long ll_null
SetNull(ll_null)
IF This.Checked THEN
	il_productor = 0
	dw_productor.SetItem(1,"prod_codigo",ll_null)
	dw_productor.Object.prod_codigo.Protect	=	1
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_productor.Object.prod_codigo.Protect	=	0
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_productor.SetFocus()
END IF
end event

type em_numdesde from editmask within w_info_reetiquetado_formatoexcel
integer x = 901
integer y = 1456
integer width = 402
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type em_numhasta from editmask within w_info_reetiquetado_formatoexcel
integer x = 1714
integer y = 1456
integer width = 402
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type gb_3 from groupbox within w_info_reetiquetado_formatoexcel
integer x = 274
integer y = 1392
integer width = 2126
integer height = 188
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nº Proceso"
end type

type gb_4 from groupbox within w_info_reetiquetado_formatoexcel
integer x = 274
integer y = 1596
integer width = 2126
integer height = 172
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Proceso"
end type

type st_12 from statictext within w_info_reetiquetado_formatoexcel
integer x = 581
integer y = 1684
integer width = 256
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
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_reetiquetado_formatoexcel
integer x = 347
integer y = 1204
integer width = 471
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type dw_embalaje from datawindow within w_info_reetiquetado_formatoexcel
integer x = 901
integer y = 1192
integer width = 1138
integer height = 92
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_embalajesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_nula
SetNull(ls_nula)
iuo_embalajesprod = create uo_embalajesprod

IF iuo_embalajesprod.Existe(gi_codexport,Data,True,SqlCa) THEN
	is_embalajes = Data
ELSE
	This.SetItem(1, "emba_codigo", ls_nula)
	RETURN 1	
END IF


end event

event itemerror;RETURN 1
end event

type cbx_todosembalaje from checkbox within w_info_reetiquetado_formatoexcel
integer x = 2062
integer y = 1196
integer width = 325
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
string text = "Todos"
boolean checked = true
end type

event clicked;String ls_nula
SetNull(ls_nula)

IF This.Checked THEN
	is_embalajes = '0'
	dw_embalaje.SetItem(1,"emba_codigo",ls_nula)
	dw_embalaje.Object.emba_codigo.protect = 1
	dw_embalaje.Object.emba_codigo.BackGround.Color = RGB(192, 192, 192)
ELSE
	is_embalajes = dw_embalaje.Object.emba_codigo[1]
	dw_embalaje.Object.emba_codigo.protect = 0
	dw_embalaje.Object.emba_codigo.BackGround.Color = RGB(255, 255, 255)
	dw_embalaje.SetFocus()
END IF
end event

type cbx_cliente from checkbox within w_info_reetiquetado_formatoexcel
integer x = 2062
integer y = 600
integer width = 311
integer height = 80
integer taborder = 50
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

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_Cliente = 0
	dw_cliente.SetItem(1,"clie_codigo",li_null)
	dw_cliente.Object.clie_codigo.Protect	=	1
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	
	IF cbx_todostipop.Checked THEN
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(0,-1)
		il_productor	=	dw_productor.Object.prod_codigo[1]
	ELSE
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(dw_tipop.Object.tipr_codigo[1],-1)
		il_productor	=	dw_productor.Object.prod_codigo[1]
	END IF	
	
ELSE
	dw_cliente.Object.clie_codigo.Protect	=	0
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_cliente.SetFocus()
END IF
end event

type cbx_planta from checkbox within w_info_reetiquetado_formatoexcel
integer x = 2062
integer y = 704
integer width = 311
integer height = 80
integer taborder = 50
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

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_Planta = 0
	dw_planta.SetItem(1,"plde_codigo",li_null)
	dw_planta.Object.plde_codigo.Protect	=	1
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_planta.Object.plde_codigo.Protect	=	0
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_planta.SetFocus()
END IF
end event

type st_1 from statictext within w_info_reetiquetado_formatoexcel
integer x = 251
integer y = 440
integer width = 2171
integer height = 1344
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_info_reetiquetado_formatoexcel
integer x = 1088
integer y = 488
integer width = 471
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
string text = "Formato Excel"
boolean checked = true
end type

type uo_selespecie from uo_seleccion_especie within w_info_reetiquetado_formatoexcel
event destroy ( )
integer x = 901
integer y = 796
integer height = 180
integer taborder = 80
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cbx_varirotula from checkbox within w_info_reetiquetado_formatoexcel
integer x = 901
integer y = 1324
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
string text = "Variedad Rotulada"
end type

