$PBExportHeader$w_info_inspeccion_historica.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_inspeccion_historica from w_para_informes
end type
type dw_cliente from datawindow within w_info_inspeccion_historica
end type
type st_2 from statictext within w_info_inspeccion_historica
end type
type st_3 from statictext within w_info_inspeccion_historica
end type
type em_fzarpe from editmask within w_info_inspeccion_historica
end type
type st_8 from statictext within w_info_inspeccion_historica
end type
type dw_planta from datawindow within w_info_inspeccion_historica
end type
type st_13 from statictext within w_info_inspeccion_historica
end type
type em_hasta from editmask within w_info_inspeccion_historica
end type
type st_14 from statictext within w_info_inspeccion_historica
end type
type st_5 from statictext within w_info_inspeccion_historica
end type
type st_9 from statictext within w_info_inspeccion_historica
end type
type st_4 from statictext within w_info_inspeccion_historica
end type
type st_7 from statictext within w_info_inspeccion_historica
end type
type st_10 from statictext within w_info_inspeccion_historica
end type
type st_11 from statictext within w_info_inspeccion_historica
end type
type dw_tipop from datawindow within w_info_inspeccion_historica
end type
type dw_productor from datawindow within w_info_inspeccion_historica
end type
type dw_destino from datawindow within w_info_inspeccion_historica
end type
type cbx_todostipop from checkbox within w_info_inspeccion_historica
end type
type cbx_todosprod from checkbox within w_info_inspeccion_historica
end type
type cbx_todosdestino from checkbox within w_info_inspeccion_historica
end type
type ddlb_tipoi from dropdownlistbox within w_info_inspeccion_historica
end type
type em_numdesde from editmask within w_info_inspeccion_historica
end type
type em_numhasta from editmask within w_info_inspeccion_historica
end type
type cbx_1 from checkbox within w_info_inspeccion_historica
end type
type gb_3 from groupbox within w_info_inspeccion_historica
end type
type gb_4 from groupbox within w_info_inspeccion_historica
end type
type st_12 from statictext within w_info_inspeccion_historica
end type
type st_15 from statictext within w_info_inspeccion_historica
end type
type dw_embalaje from datawindow within w_info_inspeccion_historica
end type
type cbx_todosembalaje from checkbox within w_info_inspeccion_historica
end type
type cbx_forexcel from checkbox within w_info_inspeccion_historica
end type
type dw_mercados from datawindow within w_info_inspeccion_historica
end type
type cbx_todosmercados from checkbox within w_info_inspeccion_historica
end type
type st_6 from statictext within w_info_inspeccion_historica
end type
type cbx_cliente from checkbox within w_info_inspeccion_historica
end type
type cbx_planta from checkbox within w_info_inspeccion_historica
end type
type gb_5 from groupbox within w_info_inspeccion_historica
end type
type st_1 from statictext within w_info_inspeccion_historica
end type
type uo_selespecie from uo_seleccion_especie within w_info_inspeccion_historica
end type
type cbx_varirotula from checkbox within w_info_inspeccion_historica
end type
type st_16 from statictext within w_info_inspeccion_historica
end type
type rb_devolucion from radiobutton within w_info_inspeccion_historica
end type
type rb_rechaza from radiobutton within w_info_inspeccion_historica
end type
type rb_anula from radiobutton within w_info_inspeccion_historica
end type
type rb_todos from radiobutton within w_info_inspeccion_historica
end type
type cbx_factur from checkbox within w_info_inspeccion_historica
end type
end forward

global type w_info_inspeccion_historica from w_para_informes
integer width = 2967
integer height = 2376
string title = "INSPECCION HISTORICA"
boolean maxbox = false
boolean resizable = false
dw_cliente dw_cliente
st_2 st_2
st_3 st_3
em_fzarpe em_fzarpe
st_8 st_8
dw_planta dw_planta
st_13 st_13
em_hasta em_hasta
st_14 st_14
st_5 st_5
st_9 st_9
st_4 st_4
st_7 st_7
st_10 st_10
st_11 st_11
dw_tipop dw_tipop
dw_productor dw_productor
dw_destino dw_destino
cbx_todostipop cbx_todostipop
cbx_todosprod cbx_todosprod
cbx_todosdestino cbx_todosdestino
ddlb_tipoi ddlb_tipoi
em_numdesde em_numdesde
em_numhasta em_numhasta
cbx_1 cbx_1
gb_3 gb_3
gb_4 gb_4
st_12 st_12
st_15 st_15
dw_embalaje dw_embalaje
cbx_todosembalaje cbx_todosembalaje
cbx_forexcel cbx_forexcel
dw_mercados dw_mercados
cbx_todosmercados cbx_todosmercados
st_6 st_6
cbx_cliente cbx_cliente
cbx_planta cbx_planta
gb_5 gb_5
st_1 st_1
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
st_16 st_16
rb_devolucion rb_devolucion
rb_rechaza rb_rechaza
rb_anula rb_anula
rb_todos rb_todos
cbx_factur cbx_factur
end type
global w_info_inspeccion_historica w_info_inspeccion_historica

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_planta, idwc_tipop,idwc_mercados,&
                     idwc_productor, idwc_destino,idwc_pesoneto,idwc_embalaje							

Integer	ii_Cliente, ii_Planta, ii_filtro,ii_tipo,ii_destinos,ii_tipoi,ii_mercado
String	is_NomEspecie, is_NomPlanta, is_NomCliente, is_embalajes
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
public function boolean existemercado (integer mercado)
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

public function boolean existemercado (integer mercado);String	ls_Nombre

SELECT	merc_nombre
	INTO	:ls_Nombre
	FROM	dbo.mercado
	WHERE	merc_codigo	=	:mercado ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Mercado")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Mercado no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	ii_mercado = mercado
	RETURN False
END IF
end function

on w_info_inspeccion_historica.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.st_3=create st_3
this.em_fzarpe=create em_fzarpe
this.st_8=create st_8
this.dw_planta=create dw_planta
this.st_13=create st_13
this.em_hasta=create em_hasta
this.st_14=create st_14
this.st_5=create st_5
this.st_9=create st_9
this.st_4=create st_4
this.st_7=create st_7
this.st_10=create st_10
this.st_11=create st_11
this.dw_tipop=create dw_tipop
this.dw_productor=create dw_productor
this.dw_destino=create dw_destino
this.cbx_todostipop=create cbx_todostipop
this.cbx_todosprod=create cbx_todosprod
this.cbx_todosdestino=create cbx_todosdestino
this.ddlb_tipoi=create ddlb_tipoi
this.em_numdesde=create em_numdesde
this.em_numhasta=create em_numhasta
this.cbx_1=create cbx_1
this.gb_3=create gb_3
this.gb_4=create gb_4
this.st_12=create st_12
this.st_15=create st_15
this.dw_embalaje=create dw_embalaje
this.cbx_todosembalaje=create cbx_todosembalaje
this.cbx_forexcel=create cbx_forexcel
this.dw_mercados=create dw_mercados
this.cbx_todosmercados=create cbx_todosmercados
this.st_6=create st_6
this.cbx_cliente=create cbx_cliente
this.cbx_planta=create cbx_planta
this.gb_5=create gb_5
this.st_1=create st_1
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
this.st_16=create st_16
this.rb_devolucion=create rb_devolucion
this.rb_rechaza=create rb_rechaza
this.rb_anula=create rb_anula
this.rb_todos=create rb_todos
this.cbx_factur=create cbx_factur
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.em_fzarpe
this.Control[iCurrent+5]=this.st_8
this.Control[iCurrent+6]=this.dw_planta
this.Control[iCurrent+7]=this.st_13
this.Control[iCurrent+8]=this.em_hasta
this.Control[iCurrent+9]=this.st_14
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.st_9
this.Control[iCurrent+12]=this.st_4
this.Control[iCurrent+13]=this.st_7
this.Control[iCurrent+14]=this.st_10
this.Control[iCurrent+15]=this.st_11
this.Control[iCurrent+16]=this.dw_tipop
this.Control[iCurrent+17]=this.dw_productor
this.Control[iCurrent+18]=this.dw_destino
this.Control[iCurrent+19]=this.cbx_todostipop
this.Control[iCurrent+20]=this.cbx_todosprod
this.Control[iCurrent+21]=this.cbx_todosdestino
this.Control[iCurrent+22]=this.ddlb_tipoi
this.Control[iCurrent+23]=this.em_numdesde
this.Control[iCurrent+24]=this.em_numhasta
this.Control[iCurrent+25]=this.cbx_1
this.Control[iCurrent+26]=this.gb_3
this.Control[iCurrent+27]=this.gb_4
this.Control[iCurrent+28]=this.st_12
this.Control[iCurrent+29]=this.st_15
this.Control[iCurrent+30]=this.dw_embalaje
this.Control[iCurrent+31]=this.cbx_todosembalaje
this.Control[iCurrent+32]=this.cbx_forexcel
this.Control[iCurrent+33]=this.dw_mercados
this.Control[iCurrent+34]=this.cbx_todosmercados
this.Control[iCurrent+35]=this.st_6
this.Control[iCurrent+36]=this.cbx_cliente
this.Control[iCurrent+37]=this.cbx_planta
this.Control[iCurrent+38]=this.gb_5
this.Control[iCurrent+39]=this.st_1
this.Control[iCurrent+40]=this.uo_selespecie
this.Control[iCurrent+41]=this.cbx_varirotula
this.Control[iCurrent+42]=this.st_16
this.Control[iCurrent+43]=this.rb_devolucion
this.Control[iCurrent+44]=this.rb_rechaza
this.Control[iCurrent+45]=this.rb_anula
this.Control[iCurrent+46]=this.rb_todos
this.Control[iCurrent+47]=this.cbx_factur
end on

on w_info_inspeccion_historica.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_fzarpe)
destroy(this.st_8)
destroy(this.dw_planta)
destroy(this.st_13)
destroy(this.em_hasta)
destroy(this.st_14)
destroy(this.st_5)
destroy(this.st_9)
destroy(this.st_4)
destroy(this.st_7)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.dw_tipop)
destroy(this.dw_productor)
destroy(this.dw_destino)
destroy(this.cbx_todostipop)
destroy(this.cbx_todosprod)
destroy(this.cbx_todosdestino)
destroy(this.ddlb_tipoi)
destroy(this.em_numdesde)
destroy(this.em_numhasta)
destroy(this.cbx_1)
destroy(this.gb_3)
destroy(this.gb_4)
destroy(this.st_12)
destroy(this.st_15)
destroy(this.dw_embalaje)
destroy(this.cbx_todosembalaje)
destroy(this.cbx_forexcel)
destroy(this.dw_mercados)
destroy(this.cbx_todosmercados)
destroy(this.st_6)
destroy(this.cbx_cliente)
destroy(this.cbx_planta)
destroy(this.gb_5)
destroy(this.st_1)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
destroy(this.st_16)
destroy(this.rb_devolucion)
destroy(this.rb_rechaza)
destroy(this.rb_anula)
destroy(this.rb_todos)
destroy(this.cbx_factur)
end on

event open;call super::open;x				= 0
y				= 0
//This.Height	= 2020

String	ls_Columna[], ls_operacion="TODAS"
Integer 	li_busca
Boolean	lb_Cerrar

dw_cliente.SetItem(-1,"clie_codigo",'')
dw_cliente.Object.clie_codigo.Protect	=	1
dw_cliente.Object.clie_codigo.Color	=	Rgb(255,255,255)
dw_cliente.Object.clie_codigo.BackGround.Color	=	553648127

dw_planta.SetItem(-1,"plde_codigo",'')
dw_planta.Object.plde_codigo.Protect	=	1
dw_planta.Object.plde_codigo.Color		=	Rgb(255,255,255)
dw_planta.Object.plde_codigo.BackGround.Color	=	553648127

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
idwc_productor.Retrieve(-1,gi_CodExport)
dw_productor.InsertRow(0)

dw_destino.GetChild("dest_codigo", idwc_destino)
idwc_destino.SetTransObject(SQLCA)
idwc_destino.Retrieve(-1)
dw_destino.InsertRow(0)

dw_embalaje.GetChild("emba_codigo", idwc_embalaje)
idwc_embalaje.SetTransObject(SQLCA)
idwc_embalaje.Retrieve(gi_CodExport)
dw_embalaje.InsertRow(0)

dw_mercados.GetChild("merc_codigo", idwc_mercados)
idwc_mercados.SetTransObject(SqlCa)
idwc_mercados.Retrieve()
dw_mercados.InsertRow(0)

ii_Cliente						=	0
ii_Planta						=	0
istr_mant.argumento[1]		= 	String(gi_codexport)		//	Cliente
istr_mant.argumento[3]		= 	String(gi_CodPlanta)		//	Planta
ii_tipo 							= 	0
ii_destinos 					= 	0

em_fzarpe.text					=	String(RelativeDate(Today() , -365))
em_hasta.text					=	String(Today())	

cbx_todostipop.Checked = True
cbx_todosprod.Checked = True
cbx_todosdestino.Checked = True

dw_tipop.Object.tipr_codigo.Protect	=	1
dw_tipop.Object.tipr_codigo.Color		=	RGB(255,255,255)
dw_tipop.Object.tipr_codigo.BackGround.Color	=	553648127

dw_productor.Object.prod_codigo.Protect				=	1
dw_productor.Object.prod_codigo.Color					=	RGB(255,255,255)
dw_productor.Object.prod_codigo.BackGround.Color	=	553648127

dw_destino.Object.dest_codigo.Protect	=	1
dw_destino.Object.dest_codigo.Color	=	RGB(255,255,255)
dw_destino.Object.dest_codigo.BackGround.Color	=	553648127
dw_embalaje.Object.emba_codigo.Protect	=	1
dw_embalaje.Object.emba_codigo.Color	=	RGB(255,255,255)
dw_embalaje.Object.emba_codigo.BackGround.Color	=	553648127
dw_mercados.Object.merc_codigo.Protect	=	1
dw_mercados.Object.merc_codigo.Color	=	RGB(255,255,255)
dw_mercados.Object.merc_codigo.BackGround.Color	=	553648127
cbx_todosmercados.Enabled = False

em_numdesde.Text = '1'
em_numhasta.Text = '99999999'

ddlb_tipoi.Item[1]='1' 
end event

type pb_excel from w_para_informes`pb_excel within w_info_inspeccion_historica
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_computador from w_para_informes`st_computador within w_info_inspeccion_historica
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_inspeccion_historica
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_inspeccion_historica
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_inspeccion_historica
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_inspeccion_historica
integer width = 2171
string text = "Informe Inspección Historico"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_inspeccion_historica
integer x = 2583
integer y = 1160
integer taborder = 160
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

event pb_acepta::clicked;Long	  	ll_Fila
String	t_fecha, is_Instructivo,ls_cajas,ls_filtra
Date 		ld_fechaZarpe
Integer	li_varirotula, li_filtro, li_factur

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME INSPECCION HISTORICA'
OpenWithParm(vinf, istr_info)

If cbx_varirotula.Checked Then
	li_varirotula = 1
Else
	li_varirotula = 0
End If

If cbx_todosprod.Checked Then
	il_productor = 0
Else
	il_productor = dw_productor.Object.prod_codigo[1]
End If

If cbx_todosembalaje.Checked Then
	is_embalajes = '0'
Else
	is_embalajes = dw_embalaje.Object.emba_codigo[1]
End If

If rb_todos.Checked Then
	li_filtro = -1
	ls_filtra = 'Todos'
ElseIf rb_anula.Checked Then
	li_filtro = 1
	ls_filtra = 'Anuladas'
ElseIf rb_rechaza.Checked Then	
	li_filtro = 2
	ls_filtra = 'Rechazados'
ElseIf rb_devolucion.Checked Then	 
	li_filtro = 3
	ls_filtra = 'Devueltos'
End If	

If cbx_factur.Checked Then
	li_factur= 1
Else
	li_factur=-1
End If

If cbx_forexcel.Checked = True Then
	vinf.dw_1.DataObject ="dw_info_inspeccion_formatoexcel"
	cbx_1.Enabled = False
	cbx_1.Checked = False
	vinf.dw_1.SetTransObject(sqlca)

	ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente, ii_Planta,uo_selespecie.Codigo,ii_tipo,il_productor, &
											 ii_tipoi,ii_destinos,Date(em_fzarpe.text),Date(em_hasta.text), &
											 Long(em_numdesde.text),Long(em_numhasta.text),is_embalajes,&
											 ii_mercado,li_varirotula,li_filtro,li_factur)
Else
	If cbx_1.Checked=True Then
		vinf.dw_1.DataObject = "dw_info_inspeccion_historicaporprod"
	Else
		vinf.dw_1.DataObject = "dw_info_inspeccion_historica"
	End If
	vinf.dw_1.SetTransObject(sqlca)
	ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente, ii_Planta, uo_selespecie.Codigo, ii_tipo, il_productor, &
										    ii_tipoi, ii_destinos, Date(em_fzarpe.text), Date(em_hasta.text), &
											 Long(em_numdesde.text), Long(em_numhasta.text), &
											 is_embalajes,li_varirotula)
End If

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("t_filtra.text = '" + ls_filtra + "'")
	vinf.dw_1.ModIfy("desde.text = '" + em_fzarpe.text + "'")
	vinf.dw_1.ModIfy("hasta.text = '" + em_hasta.text + "'")

	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_inspeccion_historica
integer x = 2578
integer y = 1476
integer taborder = 170
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type dw_cliente from datawindow within w_info_inspeccion_historica
integer x = 896
integer y = 548
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
Integer 	li_nula, li_busca

SetNull(li_nula)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	
	istr_mant.argumento[1]	=	data
	ii_cliente	=	integer(data)

   dw_cliente.Object.clie_codigo[1]	=	integer(data)
   li_busca = idwc_cliente.Find("clie_codigo = " + String(integer(data)), 1, idwc_cliente.RowCount())
   is_NomCliente = idwc_cliente.GetItemString(li_busca, "clie_nombre")

   dw_planta.GetChild("plde_codigo", idwc_planta)
   idwc_planta.SetTransObject(SQLCA)
   idwc_planta.Retrieve(1)
   dw_planta.InsertRow(0)
   dw_planta.Object.plde_codigo[1] = dw_planta.Object.plde_codigo[1]
	istr_mant.argumento[3]	=	String(dw_planta.Object.plde_codigo[1])
	ii_planta	=	integer(istr_mant.argumento[3])	
   li_busca = idwc_planta.Find("plde_codigo = " + String(ii_planta), 1, idwc_planta.RowCount())
   is_NomPlanta = idwc_planta.GetItemString(li_busca, "plde_nombre")

	dw_tipop.GetChild("tipr_codigo", idwc_tipop)
	idwc_tipop.SetTransObject(SQLCA)
	idwc_tipop.Retrieve()
	dw_tipop.InsertRow(0)
	
	IF cbx_todostipop.Checked THEN
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(0,Integer(data))
		dw_productor.InsertRow(0)
	ELSE
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(dw_tipop.Object.tipr_codigo[1],Integer(data))
		dw_productor.InsertRow(0)
	END IF	
	
	dw_destino.GetChild("dest_codigo", idwc_destino)
	idwc_destino.SetTransObject(SQLCA)
	idwc_destino.Retrieve(0)
	dw_destino.InsertRow(0)
	
	dw_embalaje.GetChild("emba_codigo", idwc_embalaje)
	idwc_embalaje.SetTransObject(SQLCA)
	idwc_embalaje.Retrieve(gi_CodExport)
	dw_embalaje.InsertRow(0)
	
	dw_mercados.GetChild("merc_codigo", idwc_mercados)
	idwc_mercados.SetTransObject(SqlCa)
	idwc_mercados.Retrieve()
	dw_mercados.InsertRow(0)

ELSE
	This.SetItem(1, "clie_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_inspeccion_historica
integer x = 343
integer y = 572
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

type st_3 from statictext within w_info_inspeccion_historica
integer x = 343
integer y = 852
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

type em_fzarpe from editmask within w_info_inspeccion_historica
integer x = 896
integer y = 1884
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

type st_8 from statictext within w_info_inspeccion_historica
integer x = 343
integer y = 676
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

type dw_planta from datawindow within w_info_inspeccion_historica
integer x = 896
integer y = 656
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

type st_13 from statictext within w_info_inspeccion_historica
integer x = 247
integer y = 2004
integer width = 2171
integer height = 104
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

type em_hasta from editmask within w_info_inspeccion_historica
integer x = 1710
integer y = 1884
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

type st_14 from statictext within w_info_inspeccion_historica
integer x = 1467
integer y = 1908
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

type st_5 from statictext within w_info_inspeccion_historica
integer x = 576
integer y = 1716
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

type st_9 from statictext within w_info_inspeccion_historica
integer x = 1467
integer y = 1716
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

type st_4 from statictext within w_info_inspeccion_historica
integer x = 343
integer y = 960
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

type st_7 from statictext within w_info_inspeccion_historica
integer x = 343
integer y = 1068
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

type st_10 from statictext within w_info_inspeccion_historica
integer x = 343
integer y = 1184
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
string text = "Tipo Inspección"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_inspeccion_historica
integer x = 343
integer y = 1292
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
string text = "Destino"
boolean focusrectangle = false
end type

type dw_tipop from datawindow within w_info_inspeccion_historica
integer x = 896
integer y = 944
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

type dw_productor from datawindow within w_info_inspeccion_historica
integer x = 896
integer y = 1052
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

type dw_destino from datawindow within w_info_inspeccion_historica
integer x = 896
integer y = 1276
integer width = 901
integer height = 100
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_destinos"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula
SetNull(li_nula)
iuo_destinos = Create uo_destinos

IF iuo_destinos.existe(Integer(data),True,SqlCa) THEN
	ii_destinos	=	integer(data)
ELSE
	This.SetItem(1, "dest_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_todostipop from checkbox within w_info_inspeccion_historica
integer x = 2057
integer y = 956
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
	dw_tipop.Object.tipr_codigo.Color	=	RGB(255,255,255)
	dw_tipop.Object.tipr_codigo.BackGround.Color	=	553848127
	
	IF cbx_cliente.Checked THEN
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(-1,-1)
		il_productor	=	dw_productor.Object.prod_codigo[1]
	ELSE
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(dw_tipop.Object.tipr_codigo[1],-1)
		il_productor	=	dw_productor.Object.prod_codigo[1]
	END IF	
ELSE
	dw_tipop.Object.tipr_codigo.Protect	=	0
	dw_tipop.Object.tipr_codigo.Color	=	0
	dw_tipop.Object.tipr_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_tipop.SetFocus()
END IF
end event

type cbx_todosprod from checkbox within w_info_inspeccion_historica
integer x = 2057
integer y = 1064
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
	dw_productor.Object.prod_codigo.Color	=	RGB(255,255,255)
	dw_productor.Object.prod_codigo.BackGround.Color	=	553648127
ELSE
	dw_productor.Object.prod_codigo.Protect	=	0
	dw_productor.Object.prod_codigo.Color	=	0
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_productor.SetFocus()
END IF
end event

type cbx_todosdestino from checkbox within w_info_inspeccion_historica
integer x = 2057
integer y = 1296
integer width = 325
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
string text = "Todos"
boolean checked = true
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_destinos = 0
	dw_destino.SetItem(1,"dest_codigo",li_null)
	dw_destino.Object.dest_codigo.Protect	=	1
	dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_destino.Object.dest_codigo.Protect	=	0
	dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_destino.SetFocus()
END IF
end event

type ddlb_tipoi from dropdownlistbox within w_info_inspeccion_historica
integer x = 896
integer y = 1160
integer width = 974
integer height = 400
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string item[] = {"Inspección","Re-Inspección"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Integer(index) = 1 THEN
	ii_tipoi = 1
ELSEIF Integer(index) = 2 THEN
	ii_tipoi = 2
END IF

ddlb_tipoi.PostEvent(Clicked!)
end event

type em_numdesde from editmask within w_info_inspeccion_historica
integer x = 901
integer y = 1692
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

type em_numhasta from editmask within w_info_inspeccion_historica
integer x = 1710
integer y = 1696
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

type cbx_1 from checkbox within w_info_inspeccion_historica
integer x = 558
integer y = 2016
integer width = 846
integer height = 80
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
string text = "Informe Por Productor"
end type

type gb_3 from groupbox within w_info_inspeccion_historica
integer x = 270
integer y = 1628
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
string text = "Nº Solicitud"
end type

type gb_4 from groupbox within w_info_inspeccion_historica
integer x = 270
integer y = 1820
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
string text = "Fecha Inspección"
end type

type st_12 from statictext within w_info_inspeccion_historica
integer x = 576
integer y = 1908
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

type st_15 from statictext within w_info_inspeccion_historica
integer x = 343
integer y = 1404
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

type dw_embalaje from datawindow within w_info_inspeccion_historica
integer x = 896
integer y = 1388
integer width = 1143
integer height = 100
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

type cbx_todosembalaje from checkbox within w_info_inspeccion_historica
integer x = 2057
integer y = 1408
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

type cbx_forexcel from checkbox within w_info_inspeccion_historica
boolean visible = false
integer x = 283
integer y = 452
integer width = 503
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
string text = "Formato Excel"
boolean checked = true
end type

event clicked;Integer li_nula, li_busca
SetNull(li_nula)

IF cbx_forexcel.Checked = True THEN
	cbx_1.Enabled = False
	cbx_1.Checked = False
	cbx_todosmercados.Enabled = True
	cbx_todosmercados.Checked = True
	ii_mercado = 0
	
	cbx_planta.Visible = True
	cbx_cliente.Visible = True
	cbx_planta.Checked = False
	cbx_cliente.Checked = False
ELSE
	cbx_1.Enabled = True
	cbx_1.Checked = False
	cbx_todosmercados.Enabled = False
	ii_mercado = 0
	dw_mercados.SetItem(1,"merc_codigo",li_nula)
	dw_mercados.Object.merc_codigo.protect = 1
	dw_mercados.Object.merc_codigo.BackGround.Color = RGB(192, 192, 192)
	
	cbx_planta.Visible = False
	cbx_cliente.Visible = False
	cbx_planta.Checked = False
	cbx_cliente.Checked = False
	
	dw_planta.Object.plde_codigo.Protect	=	0
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_planta.SetFocus()
	
	dw_cliente.Object.clie_codigo.Protect	=	0
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_cliente.SetFocus()
	
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
	
	ii_Cliente = dw_cliente.Object.clie_codigo[1]
	ii_Planta  = dw_planta.Object.plde_codigo[1]
END IF	



end event

type dw_mercados from datawindow within w_info_inspeccion_historica
integer x = 896
integer y = 1500
integer width = 901
integer height = 100
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_mercado"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula

SetNull(li_nula)

IF existemercado(Integer(data)) THEN
	This.SetItem(1,"merc_codigo", Integer(li_Nula))
	RETURN 1
END IF
end event

event itemerror;Return 1
end event

type cbx_todosmercados from checkbox within w_info_inspeccion_historica
integer x = 2057
integer y = 1520
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

event clicked;Integer li_nula
SetNull(li_nula)

IF This.Checked THEN
	ii_mercado = 0
	dw_mercados.SetItem(1,"merc_codigo",li_nula)
	dw_mercados.Object.merc_codigo.protect = 1
	dw_mercados.Object.merc_codigo.BackGround.Color = RGB(192, 192, 192)
ELSE
	ii_mercado = dw_mercados.Object.merc_codigo[1]
	dw_mercados.Object.merc_codigo.protect = 0
	dw_mercados.Object.merc_codigo.BackGround.Color = RGB(255, 255, 255)
	dw_mercados.SetFocus()
END IF
end event

type st_6 from statictext within w_info_inspeccion_historica
integer x = 343
integer y = 1516
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
string text = "Mercados"
boolean focusrectangle = false
end type

type cbx_cliente from checkbox within w_info_inspeccion_historica
integer x = 2057
integer y = 560
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
boolean checked = true
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_Cliente = 0
	dw_cliente.SetItem(1,"clie_codigo",li_null)
	dw_cliente.Object.clie_codigo.Protect	=	1
	dw_cliente.Object.clie_codigo.Color	=	Rgb(255,255,255)
	dw_cliente.Object.clie_codigo.BackGround.Color	=	553648127	
	
	IF cbx_todostipop.Checked THEN
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(0,-1)
		dw_productor.InsertRow(0)
	ELSE
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(dw_tipop.Object.tipr_codigo[1],-1)
		dw_productor.InsertRow(0)
	END IF	

ELSE
	dw_cliente.Object.clie_codigo.Protect	=	0
	dw_cliente.Object.clie_codigo.Color	=	0
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_cliente.SetFocus()
	ii_Cliente = dw_cliente.Object.clie_codigo[1]
END IF
end event

type cbx_planta from checkbox within w_info_inspeccion_historica
integer x = 2057
integer y = 668
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
boolean checked = true
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_Planta = 0
	dw_planta.SetItem(1,"plde_codigo",li_null)
	dw_planta.Object.plde_codigo.Protect	=	1
	dw_planta.Object.plde_codigo.Color		=	Rgb(255,255,255)
	dw_planta.Object.plde_codigo.BackGround.Color	=	553648127
ELSE
	dw_planta.Object.plde_codigo.Protect	=	0
	dw_planta.Object.plde_codigo.Color		=	0
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_planta.SetFocus()
	ii_Planta = dw_planta.Object.plde_codigo[1]
END IF
end event

type gb_5 from groupbox within w_info_inspeccion_historica
integer x = 270
integer y = 496
integer width = 2126
integer height = 1120
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
end type

type st_1 from statictext within w_info_inspeccion_historica
integer x = 247
integer y = 440
integer width = 2171
integer height = 1564
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

type uo_selespecie from uo_seleccion_especie within w_info_inspeccion_historica
event destroy ( )
integer x = 896
integer y = 756
integer height = 180
integer taborder = 60
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cbx_varirotula from checkbox within w_info_inspeccion_historica
integer x = 1467
integer y = 2016
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

type st_16 from statictext within w_info_inspeccion_historica
integer x = 247
integer y = 2108
integer width = 2171
integer height = 124
boolean bringtotop = true
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

type rb_devolucion from radiobutton within w_info_inspeccion_historica
integer x = 1778
integer y = 2128
integer width = 475
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
string text = "Devolucion"
end type

type rb_rechaza from radiobutton within w_info_inspeccion_historica
integer x = 1271
integer y = 2128
integer width = 475
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
string text = "Rechazados"
end type

type rb_anula from radiobutton within w_info_inspeccion_historica
integer x = 846
integer y = 2128
integer width = 393
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
string text = "Anulados"
end type

type rb_todos from radiobutton within w_info_inspeccion_historica
integer x = 466
integer y = 2128
integer width = 306
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

type cbx_factur from checkbox within w_info_inspeccion_historica
integer x = 905
integer y = 448
integer width = 745
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
string text = "Solo Las Facturables   "
boolean checked = true
end type

