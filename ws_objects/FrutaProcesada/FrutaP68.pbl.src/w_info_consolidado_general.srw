$PBExportHeader$w_info_consolidado_general.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_consolidado_general from w_para_informes
end type
type gb_3 from groupbox within w_info_consolidado_general
end type
type gb_13 from groupbox within w_info_consolidado_general
end type
type gb_4 from groupbox within w_info_consolidado_general
end type
type dw_cliente from datawindow within w_info_consolidado_general
end type
type st_2 from statictext within w_info_consolidado_general
end type
type dw_especie from datawindow within w_info_consolidado_general
end type
type dw_embarques from datawindow within w_info_consolidado_general
end type
type st_3 from statictext within w_info_consolidado_general
end type
type st_4 from statictext within w_info_consolidado_general
end type
type em_fzarpe from editmask within w_info_consolidado_general
end type
type st_8 from statictext within w_info_consolidado_general
end type
type dw_planta from datawindow within w_info_consolidado_general
end type
type st_10 from statictext within w_info_consolidado_general
end type
type dw_operaciones from datawindow within w_info_consolidado_general
end type
type st_12 from statictext within w_info_consolidado_general
end type
type rb_embalaje from radiobutton within w_info_consolidado_general
end type
type rb_variedad from radiobutton within w_info_consolidado_general
end type
type cbx_variedadrelacionada from checkbox within w_info_consolidado_general
end type
type rb_productor from radiobutton within w_info_consolidado_general
end type
type cbx_1 from checkbox within w_info_consolidado_general
end type
type cbx_2 from checkbox within w_info_consolidado_general
end type
type cbx_3 from checkbox within w_info_consolidado_general
end type
type st_17 from statictext within w_info_consolidado_general
end type
type dw_productor from datawindow within w_info_consolidado_general
end type
type cbx_embarque from checkbox within w_info_consolidado_general
end type
type cbx_productor from checkbox within w_info_consolidado_general
end type
type cbx_consolpro from checkbox within w_info_consolidado_general
end type
type cbx_operacion from checkbox within w_info_consolidado_general
end type
type cbx_planta from checkbox within w_info_consolidado_general
end type
type em_hasta from editmask within w_info_consolidado_general
end type
type st_14 from statictext within w_info_consolidado_general
end type
type st_1 from statictext within w_info_consolidado_general
end type
type st_6 from statictext within w_info_consolidado_general
end type
type st_13 from statictext within w_info_consolidado_general
end type
type dw_transp from datawindow within w_info_consolidado_general
end type
type st_5 from statictext within w_info_consolidado_general
end type
type st_15 from statictext within w_info_consolidado_general
end type
type cbx_trans from checkbox within w_info_consolidado_general
end type
type dw_tica from datawindow within w_info_consolidado_general
end type
type cbx_tica from checkbox within w_info_consolidado_general
end type
type st_7 from statictext within w_info_consolidado_general
end type
end forward

global type w_info_consolidado_general from w_para_informes
integer width = 3045
integer height = 2520
gb_3 gb_3
gb_13 gb_13
gb_4 gb_4
dw_cliente dw_cliente
st_2 st_2
dw_especie dw_especie
dw_embarques dw_embarques
st_3 st_3
st_4 st_4
em_fzarpe em_fzarpe
st_8 st_8
dw_planta dw_planta
st_10 st_10
dw_operaciones dw_operaciones
st_12 st_12
rb_embalaje rb_embalaje
rb_variedad rb_variedad
cbx_variedadrelacionada cbx_variedadrelacionada
rb_productor rb_productor
cbx_1 cbx_1
cbx_2 cbx_2
cbx_3 cbx_3
st_17 st_17
dw_productor dw_productor
cbx_embarque cbx_embarque
cbx_productor cbx_productor
cbx_consolpro cbx_consolpro
cbx_operacion cbx_operacion
cbx_planta cbx_planta
em_hasta em_hasta
st_14 st_14
st_1 st_1
st_6 st_6
st_13 st_13
dw_transp dw_transp
st_5 st_5
st_15 st_15
cbx_trans cbx_trans
dw_tica dw_tica
cbx_tica cbx_tica
st_7 st_7
end type
global w_info_consolidado_general w_info_consolidado_general

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_especie, idwc_embarque,idwc_operaciones,&
                     idwc_planta, idwc_productor,idwc_transp,idwc_tica

Integer	ii_Cliente, ii_Especie, ii_Planta, ii_Operacion, ii_agrupa, ii_var, ii_cal, ii_prod
String	is_Embarque, is_NomEspecie, is_NomEmbarque, is_NomNave, is_NomPlanta, &
			is_NomCliente, is_OPeracion, is_NomRecibidor, is_NomDestino
Long		ii_productor
Date		id_FechaZarpe


uo_transportista       iuo_transportista
uo_tipocamion			  iuo_tipocamion

end variables

forward prototypes
public function boolean existeproductor (long productor)
end prototypes

public function boolean existeproductor (long productor);String	ls_Nombre
Integer	li_cliente

li_cliente	= dw_cliente.Object.clie_Codigo[1]

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores as pro,dba.productoresclientes as cli
	WHERE	pro.prod_codigo =	:productor
	AND	pro.prod_codigo = cli.prod_codigo
	AND	:li_cliente in (-1,cli.clie_codigo);
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de productor no ha sido definido o no pertenece a cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	ii_productor = Productor
	RETURN True
END IF
end function

on w_info_consolidado_general.create
int iCurrent
call super::create
this.gb_3=create gb_3
this.gb_13=create gb_13
this.gb_4=create gb_4
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.dw_especie=create dw_especie
this.dw_embarques=create dw_embarques
this.st_3=create st_3
this.st_4=create st_4
this.em_fzarpe=create em_fzarpe
this.st_8=create st_8
this.dw_planta=create dw_planta
this.st_10=create st_10
this.dw_operaciones=create dw_operaciones
this.st_12=create st_12
this.rb_embalaje=create rb_embalaje
this.rb_variedad=create rb_variedad
this.cbx_variedadrelacionada=create cbx_variedadrelacionada
this.rb_productor=create rb_productor
this.cbx_1=create cbx_1
this.cbx_2=create cbx_2
this.cbx_3=create cbx_3
this.st_17=create st_17
this.dw_productor=create dw_productor
this.cbx_embarque=create cbx_embarque
this.cbx_productor=create cbx_productor
this.cbx_consolpro=create cbx_consolpro
this.cbx_operacion=create cbx_operacion
this.cbx_planta=create cbx_planta
this.em_hasta=create em_hasta
this.st_14=create st_14
this.st_1=create st_1
this.st_6=create st_6
this.st_13=create st_13
this.dw_transp=create dw_transp
this.st_5=create st_5
this.st_15=create st_15
this.cbx_trans=create cbx_trans
this.dw_tica=create dw_tica
this.cbx_tica=create cbx_tica
this.st_7=create st_7
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.gb_13
this.Control[iCurrent+3]=this.gb_4
this.Control[iCurrent+4]=this.dw_cliente
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.dw_especie
this.Control[iCurrent+7]=this.dw_embarques
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_4
this.Control[iCurrent+10]=this.em_fzarpe
this.Control[iCurrent+11]=this.st_8
this.Control[iCurrent+12]=this.dw_planta
this.Control[iCurrent+13]=this.st_10
this.Control[iCurrent+14]=this.dw_operaciones
this.Control[iCurrent+15]=this.st_12
this.Control[iCurrent+16]=this.rb_embalaje
this.Control[iCurrent+17]=this.rb_variedad
this.Control[iCurrent+18]=this.cbx_variedadrelacionada
this.Control[iCurrent+19]=this.rb_productor
this.Control[iCurrent+20]=this.cbx_1
this.Control[iCurrent+21]=this.cbx_2
this.Control[iCurrent+22]=this.cbx_3
this.Control[iCurrent+23]=this.st_17
this.Control[iCurrent+24]=this.dw_productor
this.Control[iCurrent+25]=this.cbx_embarque
this.Control[iCurrent+26]=this.cbx_productor
this.Control[iCurrent+27]=this.cbx_consolpro
this.Control[iCurrent+28]=this.cbx_operacion
this.Control[iCurrent+29]=this.cbx_planta
this.Control[iCurrent+30]=this.em_hasta
this.Control[iCurrent+31]=this.st_14
this.Control[iCurrent+32]=this.st_1
this.Control[iCurrent+33]=this.st_6
this.Control[iCurrent+34]=this.st_13
this.Control[iCurrent+35]=this.dw_transp
this.Control[iCurrent+36]=this.st_5
this.Control[iCurrent+37]=this.st_15
this.Control[iCurrent+38]=this.cbx_trans
this.Control[iCurrent+39]=this.dw_tica
this.Control[iCurrent+40]=this.cbx_tica
this.Control[iCurrent+41]=this.st_7
end on

on w_info_consolidado_general.destroy
call super::destroy
destroy(this.gb_3)
destroy(this.gb_13)
destroy(this.gb_4)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.dw_especie)
destroy(this.dw_embarques)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.em_fzarpe)
destroy(this.st_8)
destroy(this.dw_planta)
destroy(this.st_10)
destroy(this.dw_operaciones)
destroy(this.st_12)
destroy(this.rb_embalaje)
destroy(this.rb_variedad)
destroy(this.cbx_variedadrelacionada)
destroy(this.rb_productor)
destroy(this.cbx_1)
destroy(this.cbx_2)
destroy(this.cbx_3)
destroy(this.st_17)
destroy(this.dw_productor)
destroy(this.cbx_embarque)
destroy(this.cbx_productor)
destroy(this.cbx_consolpro)
destroy(this.cbx_operacion)
destroy(this.cbx_planta)
destroy(this.em_hasta)
destroy(this.st_14)
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_13)
destroy(this.dw_transp)
destroy(this.st_5)
destroy(this.st_15)
destroy(this.cbx_trans)
destroy(this.dw_tica)
destroy(this.cbx_tica)
destroy(this.st_7)
end on

event open;call super::open;String	ls_Columna[], ls_operacion="TODAS"


iuo_transportista			=	CREATE	uo_transportista       
iuo_tipocamion				=	CREATE   uo_tipocamion				

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.Object.clie_codigo[1]	=	gi_CodExport

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_especie.Object.espe_codigo[1]	=	gi_CodEspecie

dw_embarques.GetChild("embq_codigo", idwc_embarque)
idwc_embarque.SetTransObject(SQLCA)
idwc_embarque.Retrieve(gi_CodExport, 0)
dw_embarques.InsertRow(0)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(SQLCA)
idwc_productor.Retrieve(gi_CodExport)
dw_productor.InsertRow(0)

dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
idwc_operaciones.SetTransObject(SQLCA)
idwc_operaciones.Retrieve(gi_CodExport)
dw_operaciones.InsertRow(0)

dw_transp.GetChild("tran_codigo", idwc_transp)
idwc_transp.SetTransObject(SQLCA)
idwc_transp.Retrieve()
dw_transp.InsertRow(0)

dw_tica.GetChild("tica_codigo", idwc_tica)
idwc_tica.SetTransObject(SQLCA)
idwc_tica.Retrieve()
dw_tica.InsertRow(0)

ExisteEspecie(gi_CodExport, gi_CodEspecie, ls_Columna[])

ii_Cliente						=	gi_CodExport
ii_Especie						=	gi_CodEspecie
ii_Planta						=	0
is_embarque						=	'Z'
istr_mant.argumento[1]		= 	String(gi_codexport)		//	Cliente
istr_mant.argumento[2]		= 	String(gi_codespecie)	//	Especie
istr_mant.argumento[3]		=	'0'							// Variedad
istr_mant.argumento[4]		=	'0'							// Productor
istr_mant.argumento[5]		= 	'0'							//	Planta
istr_mant.argumento[6]		= 	'0'							//	Embarque
istr_mant.argumento[7] 		= 	String(Today())			// Fecha
istr_mant.argumento[10] 	= 	'-1'							// Transportista
istr_mant.argumento[11] 	= 	'-1'							// Tipocamion

is_NomPlanta					=	"Todas"
is_NomEmbarque					=	""
is_NomEspecie					=	ls_Columna[1]
ii_operacion					=	0
is_embarque						=	'Z'
is_Operacion					=	'Todos'

dw_embarques.Enabled			=	False
cbx_embarque.Enabled			=	False
cbx_embarque.Checked			=	True

em_fzarpe.text					=	String(Today())	
em_hasta.text					=	String(Today())	
end event

type st_computador from w_para_informes`st_computador within w_info_consolidado_general
end type

type st_usuario from w_para_informes`st_usuario within w_info_consolidado_general
end type

type st_temporada from w_para_informes`st_temporada within w_info_consolidado_general
end type

type p_logo from w_para_informes`p_logo within w_info_consolidado_general
end type

type st_titulo from w_para_informes`st_titulo within w_info_consolidado_general
integer width = 2290
integer height = 80
string text = "Informe Consolidado General de Embarques"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_consolidado_general
integer x = 2670
integer y = 1708
integer taborder = 170
end type

event pb_acepta::clicked;Long	  	ll_Fila

String	t_fecha, is_Instructivo, ls_transpor, ls_tipocamion
Date 		ld_fechaZarpe

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME CONSOLIDADO GENERAL DE EMBARQUES'

OpenWithParm(vinf, istr_info)

IF cbx_variedadrelacionada.Checked THEN
	vinf.dw_1.DataObject = "dw_info_consolidado_general_varela"
ELSE
	IF rb_embalaje.checked THEN
		vinf.dw_1.DataObject = "dw_info_consolidado_general"
	ELSEIF rb_variedad.checked THEN
		vinf.dw_1.DataObject = "dw_info_consolidado_general_var"
	ELSE
		vinf.dw_1.DataObject = "dw_info_consolidado_general_prd"
	END IF
END IF

vinf.dw_1.SetTransObject(sqlca)

IF cbx_planta.Checked  THEN
	ii_planta = 0
END IF

IF cbx_trans.Checked  THEN
	ls_transpor = 'Todos'
ELSE
	ls_transpor = iuo_transportista.nombre
END IF

IF cbx_tica.Checked  THEN
	ls_tipocamion = 'Todos'
ELSE
	ls_tipocamion = iuo_tipocamion.nombre
END IF


IF cbx_operacion.Checked THEN
	ii_operacion=0
	is_Embarque ='Z'
ELSEIF cbx_embarque.Checked THEN
		is_Instructivo = 'Todos'
	ELSE
		is_Instructivo = is_Embarque
END IF

IF cbx_productor.Checked THEN
	ii_productor	=	0
END IF

IF cbx_consolpro.Checked THEN
	ii_agrupa	=	1
ELSE
	ii_agrupa	=	0
END IF

ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente, is_Embarque, ii_Operacion, ii_Especie, &
										 ii_Planta, ii_Productor, ii_agrupa, ii_var, ii_cal,&
										 ii_prod,Date(em_fzarpe.text),Date(em_hasta.text),Integer(istr_mant.argumento[10]),Integer(istr_mant.argumento[11]))

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
	vinf.dw_1.Modify("operacion.text = '" + is_Operacion + "'")
	
	vinf.dw_1.Modify("transportista.text = '" + ls_transpor + "'")
	vinf.dw_1.Modify("tipocamion.text = '" + ls_tipocamion + "'")
	
	vinf.dw_1.Modify("embarque.text = '" + is_Instructivo + "'")
	vinf.dw_1.Modify("nave.text = '" + is_NomEmbarque + "'")
	vinf.dw_1.Modify("recibidor.text = '" + is_NomRecibidor + "'")
	vinf.dw_1.Modify("destino.text = '" + is_NomDestino + "'")
	
	vinf.dw_1.Modify("desde.text = '" + em_fzarpe.text + "'")
	vinf.dw_1.Modify("hasta.text = '" + em_hasta.text + "'")
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
end event

type pb_salir from w_para_informes`pb_salir within w_info_consolidado_general
integer x = 2670
integer y = 2048
integer taborder = 200
end type

type gb_3 from groupbox within w_info_consolidado_general
integer x = 279
integer y = 1792
integer width = 2213
integer height = 160
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Orden"
end type

type gb_13 from groupbox within w_info_consolidado_general
integer x = 279
integer y = 1924
integer width = 2030
integer height = 160
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type gb_4 from groupbox within w_info_consolidado_general
integer x = 279
integer y = 2056
integer width = 2226
integer height = 160
integer taborder = 210
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type dw_cliente from datawindow within w_info_consolidado_general
integer x = 709
integer y = 516
integer width = 1161
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
	idwc_planta.Retrieve(Integer(istr_mant.argumento[1]),1)
	istr_mant.argumento[3]	=	String(dw_planta.Object.plde_codigo[1])
	ii_planta	=	integer(istr_mant.argumento[3])

	dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
	idwc_operaciones.SetTransObject(SQLCA)
	idwc_operaciones.Retrieve(Integer(istr_mant.argumento[1]))
	istr_mant.argumento[4]	=	String(dw_operaciones.Object.oper_codigo[1])
	ii_operacion	=	integer(istr_mant.argumento[4])

	dw_embarques.GetChild("embq_codigo", idwc_embarque)
	idwc_embarque.SetTransObject(SQLCA)
	idwc_embarque.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[4]))
	istr_mant.argumento[5]	=	String(dw_embarques.Object.embq_codigo[1])
	is_embarque	=	istr_mant.argumento[5]
	
	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(SQLCA)
	idwc_productor.Retrieve(Integer(data))
	dw_productor.InsertRow(0)

ELSE
	This.SetItem(1, "clie_codigo", li_nula)
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_consolidado_general
integer x = 311
integer y = 544
integer width = 270
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
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_consolidado_general
integer x = 709
integer y = 648
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

type dw_embarques from datawindow within w_info_consolidado_general
integer x = 709
integer y = 1180
integer width = 960
integer height = 92
integer taborder = 130
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_embarques_operacion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_Nula

SetNull(ls_Nula)

IF ExistEmbarque(ii_Cliente, data, ls_Columna[]) THEN
	is_Embarque			=	ls_Columna[3]
//	is_NomRecibidor	=	ls_Columna[10]
//	is_NomDestino		=	ls_Columna[11]
	
//	is_NomEmbarque	=	ls_Columna[1] + "            " + &
//							"Fecha de Zarpe  " + ls_Columna[2] + " " + ls_Columna[4]
	//em_fzarpe.text =  ls_Columna[2]
	IF Integer(ls_Columna[5]) <> ii_Operacion THEN
		MessageBox("Atención", "Embarque corresponde a otra Operación (" + &
						ls_Columna[3] + ")")
		is_Embarque	=	""
		This.SetItem(1, "embq_codigo", ls_Nula)
						
		RETURN 1
	END IF
	
   IF ii_Operacion = 0 THEN
		dw_operaciones.SetItem(1, "oper_codigo", Integer(ls_Columna[5]))
		ii_Operacion = Integer(ls_Columna[3])
	END IF
//ELSE
//	is_Embarque	=	""
//	This.SetItem(1, "embq_codigo", ls_Nula)
//	
//	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_consolidado_general
integer x = 311
integer y = 676
integer width = 270
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_consolidado_general
integer x = 311
integer y = 1208
integer width = 311
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
string text = "Embarque"
boolean focusrectangle = false
end type

type em_fzarpe from editmask within w_info_consolidado_general
integer x = 713
integer y = 1668
integer width = 402
integer height = 92
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event em_fzarpe::modified;call super::modified;//istr_mant.argumento[7]=this.text
end event

type st_8 from statictext within w_info_consolidado_general
integer x = 311
integer y = 808
integer width = 270
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
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_consolidado_general
integer x = 709
integer y = 780
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

type st_10 from statictext within w_info_consolidado_general
integer x = 311
integer y = 1072
integer width = 315
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
string text = "Operacion"
boolean focusrectangle = false
end type

type dw_operaciones from datawindow within w_info_consolidado_general
integer x = 709
integer y = 1044
integer width = 974
integer height = 92
integer taborder = 90
boolean bringtotop = true
boolean enabled = false
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
	idwc_embarque.retrieve(ii_Cliente, ii_OPeracion)
	is_Operacion	=	String(ii_Operacion,'###') + ' ' + ls_Columna[4]
	is_NomEmbarque	=	ls_Columna[1] + ' ' +ls_Columna[2]	

END IF
end event

event itemerror;RETURN 1
end event

type st_12 from statictext within w_info_consolidado_general
integer x = 311
integer y = 940
integer width = 297
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

type rb_embalaje from radiobutton within w_info_consolidado_general
integer x = 462
integer y = 1852
integer width = 485
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Por Embalaje"
boolean checked = true
end type

type rb_variedad from radiobutton within w_info_consolidado_general
integer x = 1033
integer y = 1852
integer width = 498
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Por Variedad"
end type

type cbx_variedadrelacionada from checkbox within w_info_consolidado_general
integer x = 800
integer y = 1980
integer width = 731
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
string text = "Variedad Relacionada"
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[9]		=	'0'
	rb_embalaje.Checked			=	True
	rb_embalaje.Enabled			=	False
	rb_variedad.Enabled			=	False
ELSE
	istr_mant.argumento[9]		=	'0'
	rb_embalaje.Enabled			=	True
	rb_variedad.Enabled			=	True	
END IF
end event

type rb_productor from radiobutton within w_info_consolidado_general
integer x = 1605
integer y = 1852
integer width = 498
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Por Productor"
end type

type cbx_1 from checkbox within w_info_consolidado_general
integer x = 462
integer y = 2112
integer width = 480
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
string text = "Variedad Rot."
end type

event clicked;IF this.checked= true THEN
	ii_var = 1
ELSE
	ii_var = 0
END IF	
end event

type cbx_2 from checkbox within w_info_consolidado_general
integer x = 1033
integer y = 2116
integer width = 439
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
string text = "Calidad Rot."
end type

event clicked;IF this.checked= true THEN
	ii_cal = 1
ELSE
	ii_cal = 0
END IF	
end event

type cbx_3 from checkbox within w_info_consolidado_general
integer x = 1605
integer y = 2112
integer width = 498
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
string text = "Productor Rot."
end type

event clicked;IF this.checked= true THEN
	ii_prod = 1
ELSE
	ii_prod = 0
END IF	
end event

type st_17 from statictext within w_info_consolidado_general
integer x = 251
integer y = 1788
integer width = 2290
integer height = 464
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_productor from datawindow within w_info_consolidado_general
integer x = 709
integer y = 912
integer width = 965
integer height = 92
integer taborder = 70
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores_clientes"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Nula

SetNull(li_Nula)

IF ExisteProductor(Long(data)) THEN
	ii_Productor		=	Long(data)	
ELSE
	This.SetItem(1, "prod_codigo", li_Nula)
	idwc_productor.retrieve()
	RETURN 1
END IF
end event

type cbx_embarque from checkbox within w_info_consolidado_general
integer x = 1755
integer y = 1200
integer width = 288
integer height = 72
integer taborder = 100
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

event clicked;IF This.Checked THEN
	is_embarque	=	'Z'
	dw_embarques.Enabled	=	False
	dw_embarques.Reset()
	dw_embarques.InsertRow(0)
ELSE
	dw_embarques.Enabled	=	True
	dw_embarques.SetFocus()
END IF
end event

type cbx_productor from checkbox within w_info_consolidado_general
integer x = 1755
integer y = 928
integer width = 288
integer height = 72
integer taborder = 50
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

event clicked;IF This.Checked THEN
	dw_productor.Enabled		=	False
	cbx_consolpro.Enabled	=	False
ELSE
	dw_productor.Enabled		=	True
	dw_productor.Reset()
	dw_productor.InsertRow(0)
	dw_productor.SetFocus()
	cbx_consolpro.Enabled	=	True
END IF	
end event

type cbx_consolpro from checkbox within w_info_consolidado_general
integer x = 2048
integer y = 928
integer width = 475
integer height = 72
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.checked=True THEN
	dw_productor.Enabled=False
ELSE
	dw_productor.Enabled=True
	dw_productor.Reset()
	dw_productor.InsertRow(0)
	dw_productor.SetFocus()
END IF
	
end event

type cbx_operacion from checkbox within w_info_consolidado_general
integer x = 1755
integer y = 1064
integer width = 288
integer height = 72
integer taborder = 80
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

event clicked;IF This.Checked THEN
	ii_Operacion				=	0
	is_embarque					=	"Z"
	is_operacion				=	"Todos"
	is_nomembarque				=  "Todos"
	dw_operaciones.Enabled	=	False
	dw_operaciones.Enabled	=	False
	dw_embarques.Enabled		=	False
	cbx_embarque.Enabled		=	False
	cbx_embarque.Checked		=	True
	dw_operaciones.Reset()
   dw_operaciones.InsertRow(0)
	dw_embarques.Reset()
   dw_embarques.InsertRow(0)
ELSE
	dw_operaciones.Enabled	=	True
	cbx_embarque.Enabled		=	True
	cbx_embarque.Checked		=	True
	dw_operaciones.SetFocus()
END IF
end event

type cbx_planta from checkbox within w_info_consolidado_general
integer x = 1755
integer y = 796
integer width = 288
integer height = 76
integer taborder = 30
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

type em_hasta from editmask within w_info_consolidado_general
integer x = 1710
integer y = 1668
integer width = 402
integer height = 92
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_14 from statictext within w_info_consolidado_general
integer x = 1440
integer y = 1696
integer width = 206
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
string text = "Hasta"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_consolidado_general
integer x = 251
integer y = 440
integer width = 2290
integer height = 876
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_consolidado_general
integer x = 315
integer y = 1696
integer width = 411
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Fecha Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_consolidado_general
integer x = 251
integer y = 1644
integer width = 2290
integer height = 144
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_transp from datawindow within w_info_consolidado_general
integer x = 713
integer y = 1376
integer width = 896
integer height = 92
integer taborder = 140
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

type st_5 from statictext within w_info_consolidado_general
integer x = 315
integer y = 1400
integer width = 389
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Transportista"
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_consolidado_general
integer x = 251
integer y = 1320
integer width = 2290
integer height = 320
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_trans from checkbox within w_info_consolidado_general
integer x = 1760
integer y = 1396
integer width = 288
integer height = 72
integer taborder = 110
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

type dw_tica from datawindow within w_info_consolidado_general
integer x = 713
integer y = 1512
integer width = 983
integer height = 92
integer taborder = 220
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

type cbx_tica from checkbox within w_info_consolidado_general
integer x = 1760
integer y = 1512
integer width = 288
integer height = 72
integer taborder = 120
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

type st_7 from statictext within w_info_consolidado_general
integer x = 315
integer y = 1520
integer width = 389
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
string text = "Tipo Camión"
boolean focusrectangle = false
end type

