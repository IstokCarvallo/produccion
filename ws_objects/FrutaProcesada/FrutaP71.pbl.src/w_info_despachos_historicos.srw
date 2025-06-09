$PBExportHeader$w_info_despachos_historicos.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_despachos_historicos from w_para_informes
end type
type dw_cliente from datawindow within w_info_despachos_historicos
end type
type st_2 from statictext within w_info_despachos_historicos
end type
type st_3 from statictext within w_info_despachos_historicos
end type
type em_fzarpe from editmask within w_info_despachos_historicos
end type
type st_8 from statictext within w_info_despachos_historicos
end type
type dw_planta from datawindow within w_info_despachos_historicos
end type
type em_hasta from editmask within w_info_despachos_historicos
end type
type st_14 from statictext within w_info_despachos_historicos
end type
type st_5 from statictext within w_info_despachos_historicos
end type
type st_9 from statictext within w_info_despachos_historicos
end type
type st_4 from statictext within w_info_despachos_historicos
end type
type st_7 from statictext within w_info_despachos_historicos
end type
type st_10 from statictext within w_info_despachos_historicos
end type
type st_11 from statictext within w_info_despachos_historicos
end type
type dw_tipop from datawindow within w_info_despachos_historicos
end type
type dw_productor from datawindow within w_info_despachos_historicos
end type
type dw_destino from datawindow within w_info_despachos_historicos
end type
type cbx_todostipop from checkbox within w_info_despachos_historicos
end type
type cbx_todosprod from checkbox within w_info_despachos_historicos
end type
type cbx_todosdestino from checkbox within w_info_despachos_historicos
end type
type em_numdesde from editmask within w_info_despachos_historicos
end type
type em_numhasta from editmask within w_info_despachos_historicos
end type
type cbx_1 from checkbox within w_info_despachos_historicos
end type
type st_12 from statictext within w_info_despachos_historicos
end type
type cbx_2 from checkbox within w_info_despachos_historicos
end type
type cbx_3 from checkbox within w_info_despachos_historicos
end type
type ddlb_tipoi from dropdownlistbox within w_info_despachos_historicos
end type
type cbx_4 from checkbox within w_info_despachos_historicos
end type
type cbx_5 from checkbox within w_info_despachos_historicos
end type
type gb_6 from groupbox within w_info_despachos_historicos
end type
type st_6 from statictext within w_info_despachos_historicos
end type
type st_15 from statictext within w_info_despachos_historicos
end type
type dw_camion from datawindow within w_info_despachos_historicos
end type
type dw_embalaje from datawindow within w_info_despachos_historicos
end type
type cbx_todoscamion from checkbox within w_info_despachos_historicos
end type
type cbx_todosembalaje from checkbox within w_info_despachos_historicos
end type
type cbx_forexcel from checkbox within w_info_despachos_historicos
end type
type cbx_cliente from checkbox within w_info_despachos_historicos
end type
type cbx_planta from checkbox within w_info_despachos_historicos
end type
type st_16 from statictext within w_info_despachos_historicos
end type
type st_17 from statictext within w_info_despachos_historicos
end type
type st_1 from statictext within w_info_despachos_historicos
end type
type em_guiaini from editmask within w_info_despachos_historicos
end type
type em_guiafin from editmask within w_info_despachos_historicos
end type
type dw_puerto from datawindow within w_info_despachos_historicos
end type
type cbx_todospuerto from checkbox within w_info_despachos_historicos
end type
type cbx_conspuerto from checkbox within w_info_despachos_historicos
end type
type cbx_6 from checkbox within w_info_despachos_historicos
end type
type uo_selespecie from uo_seleccion_especie within w_info_despachos_historicos
end type
type cbx_varirotula from checkbox within w_info_despachos_historicos
end type
type st_plantadestino from statictext within w_info_despachos_historicos
end type
type dw_plantadestino from datawindow within w_info_despachos_historicos
end type
type cbx_7 from checkbox within w_info_despachos_historicos
end type
type st_18 from statictext within w_info_despachos_historicos
end type
type dw_packing from datawindow within w_info_despachos_historicos
end type
type cbx_packing from checkbox within w_info_despachos_historicos
end type
type cbx_packingcons from checkbox within w_info_despachos_historicos
end type
type st_19 from statictext within w_info_despachos_historicos
end type
type st_31 from statictext within w_info_despachos_historicos
end type
type dw_exportador from datawindow within w_info_despachos_historicos
end type
type cbx_8 from checkbox within w_info_despachos_historicos
end type
type st_13 from statictext within w_info_despachos_historicos
end type
type cbx_tipoi from checkbox within w_info_despachos_historicos
end type
end forward

global type w_info_despachos_historicos from w_para_informes
integer width = 3374
integer height = 2664
string title = "INFORMES DESPACHOS"
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
st_10 st_10
st_11 st_11
dw_tipop dw_tipop
dw_productor dw_productor
dw_destino dw_destino
cbx_todostipop cbx_todostipop
cbx_todosprod cbx_todosprod
cbx_todosdestino cbx_todosdestino
em_numdesde em_numdesde
em_numhasta em_numhasta
cbx_1 cbx_1
st_12 st_12
cbx_2 cbx_2
cbx_3 cbx_3
ddlb_tipoi ddlb_tipoi
cbx_4 cbx_4
cbx_5 cbx_5
gb_6 gb_6
st_6 st_6
st_15 st_15
dw_camion dw_camion
dw_embalaje dw_embalaje
cbx_todoscamion cbx_todoscamion
cbx_todosembalaje cbx_todosembalaje
cbx_forexcel cbx_forexcel
cbx_cliente cbx_cliente
cbx_planta cbx_planta
st_16 st_16
st_17 st_17
st_1 st_1
em_guiaini em_guiaini
em_guiafin em_guiafin
dw_puerto dw_puerto
cbx_todospuerto cbx_todospuerto
cbx_conspuerto cbx_conspuerto
cbx_6 cbx_6
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
st_plantadestino st_plantadestino
dw_plantadestino dw_plantadestino
cbx_7 cbx_7
st_18 st_18
dw_packing dw_packing
cbx_packing cbx_packing
cbx_packingcons cbx_packingcons
st_19 st_19
st_31 st_31
dw_exportador dw_exportador
cbx_8 cbx_8
st_13 st_13
cbx_tipoi cbx_tipoi
end type
global w_info_despachos_historicos w_info_despachos_historicos

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_planta, idwc_tipop,&
                     idwc_productor, idwc_destino,idwc_pesoneto, idwc_camion,&
                     idwc_embalaje, idwc_puerto,idwc_plantadestino,idwc_packing, idwc_exportador
							
Integer	ii_Cliente, ii_Planta, ii_filtro,ii_tipo,ii_destinos,ii_tipoi,ii_camion,ii_PlantaDestino, ii_packing, ii_exportador
String	is_NomPlanta, is_NomCliente, is_embalajes, is_NomPlantaDestino		
Long		il_productor
Date		id_FechaZarpe

uo_productores     		iuo_productores   
uo_tipoproductor   		iuo_tipoproductor
uo_destinos        		iuo_destinos
uo_Embalajesprod   		iuo_Embalajesprod
uo_tipocamion      		iuo_tipocamion 
uo_puertos		    		iuo_puertos
uo_seleccion_especie		iuo_selespecie

end variables

forward prototypes
public function boolean existeproductor (long productor)
public function boolean existepacking (integer li_planta)
public function boolean existe_exportador (integer ai_exportador)
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

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dbo.plantadesp
WHERE	plde_codigo =  :li_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	//istr_mant.argumento[7] = String(li_planta)
	RETURN True 
END IF
end function

public function boolean existe_exportador (integer ai_exportador);Integer  li_cont

SELECT	count(*)
INTO    :li_cont
FROM	dbo.exportadores
WHERE	 expo_codigo =  :ai_exportador;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Exportadores")
	
	RETURN True
ELSEIF li_cont = 0 THEN
	MessageBox("Atención", "Código de Exportador no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
END IF
end function

on w_info_despachos_historicos.create
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
this.st_10=create st_10
this.st_11=create st_11
this.dw_tipop=create dw_tipop
this.dw_productor=create dw_productor
this.dw_destino=create dw_destino
this.cbx_todostipop=create cbx_todostipop
this.cbx_todosprod=create cbx_todosprod
this.cbx_todosdestino=create cbx_todosdestino
this.em_numdesde=create em_numdesde
this.em_numhasta=create em_numhasta
this.cbx_1=create cbx_1
this.st_12=create st_12
this.cbx_2=create cbx_2
this.cbx_3=create cbx_3
this.ddlb_tipoi=create ddlb_tipoi
this.cbx_4=create cbx_4
this.cbx_5=create cbx_5
this.gb_6=create gb_6
this.st_6=create st_6
this.st_15=create st_15
this.dw_camion=create dw_camion
this.dw_embalaje=create dw_embalaje
this.cbx_todoscamion=create cbx_todoscamion
this.cbx_todosembalaje=create cbx_todosembalaje
this.cbx_forexcel=create cbx_forexcel
this.cbx_cliente=create cbx_cliente
this.cbx_planta=create cbx_planta
this.st_16=create st_16
this.st_17=create st_17
this.st_1=create st_1
this.em_guiaini=create em_guiaini
this.em_guiafin=create em_guiafin
this.dw_puerto=create dw_puerto
this.cbx_todospuerto=create cbx_todospuerto
this.cbx_conspuerto=create cbx_conspuerto
this.cbx_6=create cbx_6
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
this.st_plantadestino=create st_plantadestino
this.dw_plantadestino=create dw_plantadestino
this.cbx_7=create cbx_7
this.st_18=create st_18
this.dw_packing=create dw_packing
this.cbx_packing=create cbx_packing
this.cbx_packingcons=create cbx_packingcons
this.st_19=create st_19
this.st_31=create st_31
this.dw_exportador=create dw_exportador
this.cbx_8=create cbx_8
this.st_13=create st_13
this.cbx_tipoi=create cbx_tipoi
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
this.Control[iCurrent+13]=this.st_10
this.Control[iCurrent+14]=this.st_11
this.Control[iCurrent+15]=this.dw_tipop
this.Control[iCurrent+16]=this.dw_productor
this.Control[iCurrent+17]=this.dw_destino
this.Control[iCurrent+18]=this.cbx_todostipop
this.Control[iCurrent+19]=this.cbx_todosprod
this.Control[iCurrent+20]=this.cbx_todosdestino
this.Control[iCurrent+21]=this.em_numdesde
this.Control[iCurrent+22]=this.em_numhasta
this.Control[iCurrent+23]=this.cbx_1
this.Control[iCurrent+24]=this.st_12
this.Control[iCurrent+25]=this.cbx_2
this.Control[iCurrent+26]=this.cbx_3
this.Control[iCurrent+27]=this.ddlb_tipoi
this.Control[iCurrent+28]=this.cbx_4
this.Control[iCurrent+29]=this.cbx_5
this.Control[iCurrent+30]=this.gb_6
this.Control[iCurrent+31]=this.st_6
this.Control[iCurrent+32]=this.st_15
this.Control[iCurrent+33]=this.dw_camion
this.Control[iCurrent+34]=this.dw_embalaje
this.Control[iCurrent+35]=this.cbx_todoscamion
this.Control[iCurrent+36]=this.cbx_todosembalaje
this.Control[iCurrent+37]=this.cbx_forexcel
this.Control[iCurrent+38]=this.cbx_cliente
this.Control[iCurrent+39]=this.cbx_planta
this.Control[iCurrent+40]=this.st_16
this.Control[iCurrent+41]=this.st_17
this.Control[iCurrent+42]=this.st_1
this.Control[iCurrent+43]=this.em_guiaini
this.Control[iCurrent+44]=this.em_guiafin
this.Control[iCurrent+45]=this.dw_puerto
this.Control[iCurrent+46]=this.cbx_todospuerto
this.Control[iCurrent+47]=this.cbx_conspuerto
this.Control[iCurrent+48]=this.cbx_6
this.Control[iCurrent+49]=this.uo_selespecie
this.Control[iCurrent+50]=this.cbx_varirotula
this.Control[iCurrent+51]=this.st_plantadestino
this.Control[iCurrent+52]=this.dw_plantadestino
this.Control[iCurrent+53]=this.cbx_7
this.Control[iCurrent+54]=this.st_18
this.Control[iCurrent+55]=this.dw_packing
this.Control[iCurrent+56]=this.cbx_packing
this.Control[iCurrent+57]=this.cbx_packingcons
this.Control[iCurrent+58]=this.st_19
this.Control[iCurrent+59]=this.st_31
this.Control[iCurrent+60]=this.dw_exportador
this.Control[iCurrent+61]=this.cbx_8
this.Control[iCurrent+62]=this.st_13
this.Control[iCurrent+63]=this.cbx_tipoi
end on

on w_info_despachos_historicos.destroy
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
destroy(this.st_10)
destroy(this.st_11)
destroy(this.dw_tipop)
destroy(this.dw_productor)
destroy(this.dw_destino)
destroy(this.cbx_todostipop)
destroy(this.cbx_todosprod)
destroy(this.cbx_todosdestino)
destroy(this.em_numdesde)
destroy(this.em_numhasta)
destroy(this.cbx_1)
destroy(this.st_12)
destroy(this.cbx_2)
destroy(this.cbx_3)
destroy(this.ddlb_tipoi)
destroy(this.cbx_4)
destroy(this.cbx_5)
destroy(this.gb_6)
destroy(this.st_6)
destroy(this.st_15)
destroy(this.dw_camion)
destroy(this.dw_embalaje)
destroy(this.cbx_todoscamion)
destroy(this.cbx_todosembalaje)
destroy(this.cbx_forexcel)
destroy(this.cbx_cliente)
destroy(this.cbx_planta)
destroy(this.st_16)
destroy(this.st_17)
destroy(this.st_1)
destroy(this.em_guiaini)
destroy(this.em_guiafin)
destroy(this.dw_puerto)
destroy(this.cbx_todospuerto)
destroy(this.cbx_conspuerto)
destroy(this.cbx_6)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
destroy(this.st_plantadestino)
destroy(this.dw_plantadestino)
destroy(this.cbx_7)
destroy(this.st_18)
destroy(this.dw_packing)
destroy(this.cbx_packing)
destroy(this.cbx_packingcons)
destroy(this.st_19)
destroy(this.st_31)
destroy(this.dw_exportador)
destroy(this.cbx_8)
destroy(this.st_13)
destroy(this.cbx_tipoi)
end on

event open;call super::open;x=0
y=0
String	ls_Columna[], ls_operacion="TODAS", ls_filtro
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


dw_plantadestino.GetChild("plde_codigo", idwc_plantadestino)
idwc_plantadestino.SetTransObject(SQLCA)
idwc_plantadestino.Retrieve(1)
dw_plantadestino.InsertRow(0)

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

ExisteEspecie(gi_CodExport, gi_CodEspecie, ls_Columna[])

dw_tipop.GetChild("tipr_codigo", idwc_tipop)
idwc_tipop.SetTransObject(SQLCA)
idwc_tipop.Retrieve()
dw_tipop.InsertRow(0)
//dw_tipop.Object.tipr_codigo[1]	=	ii_tipo

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(SQLCA)
idwc_productor.Retrieve(0,gi_CodExport)
dw_productor.InsertRow(0)

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2)
dw_packing.InsertRow(0)

dw_packing.Enabled											=	False
dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_puerto.GetChild("puer_codigo", idwc_puerto)
idwc_puerto.SetTransObject(SQLCA)
idwc_puerto.Retrieve(0)
dw_puerto.InsertRow(0)

dw_destino.GetChild("dest_codigo", idwc_destino)
idwc_destino.SetTransObject(SQLCA)
idwc_destino.Retrieve(0)
dw_destino.InsertRow(0)
//dw_destino.Object.dest_codigo[1]	=	ii_destino

dw_camion.GetChild("tica_codigo", idwc_camion)
idwc_camion.SetTransObject(SQLCA)
idwc_camion.Retrieve()
dw_camion.InsertRow(0)

dw_exportador.GetChild("expo_codigo", idwc_exportador)
idwc_exportador.SetTransObject(SQLCA)
idwc_exportador.Retrieve()
dw_exportador.InsertRow(0)

dw_exportador.Enabled											=	False
dw_exportador.Object.expo_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_embalaje.GetChild("emba_codigo", idwc_embalaje)
idwc_embalaje.SetTransObject(SQLCA)
idwc_embalaje.Retrieve(gi_CodExport)
dw_embalaje.InsertRow(0)

//ls_Filtro = "Mid(emba_codigo,1,1) = 'U'"
//idwc_embalaje.SetFilter(ls_Filtro)
//idwc_embalaje.Filter()

ii_Cliente						=	gi_CodExport
ii_Planta						=	gi_CodPlanta
istr_mant.argumento[1]		= 	String(gi_codexport)		//	Cliente
istr_mant.argumento[3]		= 	String(gi_CodPlanta)		//	Planta
ii_tipo 							= 	0
il_productor 					= 	0
ii_destinos 					= 	0
ii_camion 						= 	0
is_embalajes 					= 	'0'
ii_packing						= 	-9
ii_exportador					= 	-1

em_fzarpe.text					=	String(RelativeDate(Today() , -365))
em_hasta.text					=	String(Today())	

cbx_todostipop.Checked = True
cbx_todosprod.Checked = True
cbx_todosdestino.Checked = True
dw_tipop.Object.tipr_codigo.Protect	=	1
dw_tipop.Object.tipr_codigo.Color	=	RGB(255,255,255)
dw_tipop.Object.tipr_codigo.BackGround.Color	=	553648127
dw_productor.Object.prod_codigo.Protect	=	1
dw_productor.Object.prod_codigo.Color	=	RGB(255,255,255)
dw_productor.Object.prod_codigo.BackGround.Color	=	553648127
dw_destino.Object.dest_codigo.Protect	=	1
dw_destino.Object.dest_codigo.Color	=	RGB(255,255,255)
dw_destino.Object.dest_codigo.BackGround.Color	=	553648127
dw_camion.Object.tica_codigo.Protect	=	1
dw_camion.Object.tica_codigo.Color	=	RGB(255,255,255)
dw_camion.Object.tica_codigo.BackGround.Color	=	553648127
dw_embalaje.Object.emba_codigo.Protect	=	1
dw_embalaje.Object.emba_codigo.Color	=	RGB(255,255,255)
dw_embalaje.Object.emba_codigo.BackGround.Color	=	553648127

dw_plantadestino.Enabled	=	False
dw_plantadestino.Object.plde_codigo.Protect	=	1
dw_plantadestino.Object.plde_codigo.Color	=	RGB(255,255,255)
dw_plantadestino.Object.plde_codigo.BackGround.Color	=	553648127


iuo_puertos				=	Create uo_puertos

em_numdesde.Text = '1'
em_numhasta.Text = '99999999'

em_guiaini.Text = '1'
em_guiafin.Text = '99999999'
end event

type pb_excel from w_para_informes`pb_excel within w_info_despachos_historicos
integer x = 2958
integer y = 632
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_computador from w_para_informes`st_computador within w_info_despachos_historicos
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_despachos_historicos
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_despachos_historicos
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_despachos_historicos
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_despachos_historicos
integer width = 2592
integer height = 104
string text = "Informes de Despachos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_despachos_historicos
integer x = 2944
integer y = 1784
integer taborder = 300
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

event pb_acepta::clicked;Long	  	ll_Fila, ll_productor
Integer 	li_puertoori, li_varirotula
String 	t_fecha, is_Instructivo,ls_cajas,ls_productor
Date 		ld_fechaZarpe

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME DESPACHOS'

OpenWithParm(vinf, istr_info)
/*
Especies
*/
If IsNull(uo_selespecie.Codigo)Then
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_SelEspecie.dw_Seleccion.SetFocus()
	RETURN
End If

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

If cbx_conspuerto.Checked  Then
	li_puertoori = -9
ElseIf cbx_todospuerto.Checked Then
	li_puertoori = -1
Else	
	li_puertoori = dw_puerto.Object.puer_codigo[1]
End If

If cbx_forexcel.Checked = True Then
	vinf.dw_1.DataObject ="dw_info_despacho_formatoexcel"
	cbx_1.Enabled = False
	cbx_2.Enabled = False
	cbx_3.Enabled = False
	cbx_4.Enabled = False
	cbx_5.Enabled = False
	cbx_1.Checked = False
	cbx_2.Checked = False
	cbx_3.Checked = False
	cbx_4.Checked = False
	cbx_5.Checked = False
	cbx_6.Enabled = False
	cbx_6.Checked = False	
	cbx_varirotula.Enabled = False
	cbx_varirotula.Checked = False
Else
	If cbx_1.Checked Then
		If cbx_6.Checked Then
			vinf.dw_1.DataObject = "dw_info_adm_repalletizajefriorec_prodcon"
		Else
			vinf.dw_1.DataObject = "dw_info_adm_repalletizajefriodes_prod"
		End If
	Else
		vinf.dw_1.DataObject = "dw_info_adm_repalletizajefriodes"
	End If
	If cbx_2.Checked Then
	   vinf.dw_1.DataObject = "dw_info_adm_repalfriodesemb"
	ElseIf cbx_3.Checked Then
		vinf.dw_1.DataObject = "dw_info_adm_repalfriodesemb_prod"
	ElseIf cbx_4.Checked Then
		vinf.dw_1.DataObject = "dw_info_despacho_historico"
	ElseIf cbx_5.Checked Then
		vinf.dw_1.DataObject = "dw_info_despachohistoricoporprod"
	End If	
End If
vinf.dw_1.SetTransObject(sqlca)

If cbx_forexcel.Checked Then
	ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente, ii_Planta,uo_selespecie.Codigo,ii_tipo,il_productor,&
                           ii_tipoi,ii_destinos,Date(em_fzarpe.text),Date(em_hasta.text), Long(em_numdesde.text),Long(em_numhasta.text),&
						ii_camion,is_embalajes, Long(em_guiaini.text),Long(em_guiafin.text),li_puertoori,li_varirotula,ii_PlantaDestino,ii_packing,ii_exportador)
Else										 
	ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente, ii_Planta,uo_selespecie.Codigo,ii_tipo,il_productor,&
							ii_tipoi,ii_destinos,Date(em_fzarpe.text),Date(em_hasta.text),Long(em_numdesde.text),&
							Long(em_numhasta.text),ii_camion, is_embalajes,li_varirotula)
End If

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.",  StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.ModIfy("desde.text = '" + em_fzarpe.text + "'")
	vinf.dw_1.ModIfy("hasta.text = '" + em_hasta.text + "'")
		
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_despachos_historicos
integer x = 2944
integer y = 2096
integer taborder = 320
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type dw_cliente from datawindow within w_info_despachos_historicos
integer x = 1015
integer y = 476
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
	ELSE
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(dw_tipop.Object.tipr_codigo[1],dw_cliente.Object.clie_codigo[1])
	END IF	

ELSE
	This.SetItem(1, "clie_codigo", li_nula)
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 500
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

type st_3 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 732
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

type em_fzarpe from editmask within w_info_despachos_historicos
integer x = 1019
integer y = 1844
integer width = 402
integer height = 92
integer taborder = 230
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

type st_8 from statictext within w_info_despachos_historicos
integer x = 311
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

type dw_planta from datawindow within w_info_despachos_historicos
integer x = 1019
integer y = 568
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

type em_hasta from editmask within w_info_despachos_historicos
integer x = 2107
integer y = 1844
integer width = 402
integer height = 92
integer taborder = 250
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

type st_14 from statictext within w_info_despachos_historicos
integer x = 1865
integer y = 1868
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

type st_5 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 1624
integer width = 590
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
string text = "No.Despacho Desde"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_despachos_historicos
integer x = 1865
integer y = 1624
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

type st_4 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 844
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

type st_7 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 944
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

type st_10 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 1044
integer width = 416
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
string text = "Tipo Salida"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 1324
integer width = 288
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

type dw_tipop from datawindow within w_info_despachos_historicos
integer x = 1019
integer y = 828
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
	ELSE
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(Integer(Data),dw_cliente.Object.clie_codigo[1])
	END IF
	
ELSE
	This.SetItem(1, "tipr_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type dw_productor from datawindow within w_info_despachos_historicos
integer x = 1019
integer y = 924
integer width = 1006
integer height = 92
integer taborder = 80
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

type dw_destino from datawindow within w_info_despachos_historicos
integer x = 1019
integer y = 1304
integer width = 901
integer height = 92
integer taborder = 130
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

type cbx_todostipop from checkbox within w_info_despachos_historicos
integer x = 2213
integer y = 832
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
	ii_tipo = 0
	dw_tipop.SetItem(1,"tipr_codigo",li_null)
	dw_tipop.Object.tipr_codigo.Protect	=	1
	dw_tipop.Object.tipr_codigo.BackGround.Color	=	RGB(166,180,210)
	
	IF cbx_cliente.Checked THEN
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(0,-1)
	ELSE
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(0,dw_cliente.Object.clie_codigo[1])
	END IF	
	
ELSE
	dw_tipop.Object.tipr_codigo.Protect	=	0
	dw_tipop.Object.tipr_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_tipop.SetFocus()
END IF
end event

type cbx_todosprod from checkbox within w_info_despachos_historicos
integer x = 2213
integer y = 928
integer width = 297
integer height = 80
integer taborder = 90
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

type cbx_todosdestino from checkbox within w_info_despachos_historicos
integer x = 2213
integer y = 1300
integer width = 325
integer height = 80
integer taborder = 140
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

type em_numdesde from editmask within w_info_despachos_historicos
integer x = 1019
integer y = 1604
integer width = 402
integer height = 92
integer taborder = 190
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

type em_numhasta from editmask within w_info_despachos_historicos
integer x = 2112
integer y = 1604
integer width = 402
integer height = 92
integer taborder = 210
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

type cbx_1 from checkbox within w_info_despachos_historicos
integer x = 366
integer y = 1964
integer width = 786
integer height = 72
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

event clicked;IF This.Checked THEN
	cbx_2.Checked = False
	cbx_3.Checked = False
	cbx_4.Checked = False
	cbx_5.Checked = False
	cbx_6.Enabled = True
	cbx_6.Checked = True
ELSE
	cbx_6.Enabled = False
	cbx_6.Checked = False
END IF
end event

type st_12 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 1868
integer width = 494
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
string text = "Despacho Desde"
boolean focusrectangle = false
end type

type cbx_2 from checkbox within w_info_despachos_historicos
boolean visible = false
integer x = 283
integer y = 2092
integer width = 727
integer height = 80
integer taborder = 260
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Despacho Embalaje"
end type

event clicked;IF This.Checked THEN
	cbx_1.Checked = False
	cbx_3.Checked = False
	cbx_4.Checked = False
	cbx_5.Checked = False
	cbx_6.Enabled = False
	cbx_6.Checked = False
END IF
end event

type cbx_3 from checkbox within w_info_despachos_historicos
boolean visible = false
integer x = 283
integer y = 2160
integer width = 745
integer height = 80
integer taborder = 270
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desp.Embj.xProductor"
end type

event clicked;IF This.Checked THEN
	cbx_1.Checked = False
	cbx_2.Checked = False
	cbx_4.Checked = False
	cbx_5.Checked = False
	cbx_6.Enabled = False
	cbx_6.Checked = False
END IF
end event

type ddlb_tipoi from dropdownlistbox within w_info_despachos_historicos
integer x = 1019
integer y = 1012
integer width = 1006
integer height = 692
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean vscrollbar = true
string item[] = {"  2. Reproceso","  3. Reembalaje","  5. Reproceso Serv. 3º","  6. Reembalaje Serv.3ª","  7. Embarque Marítimo","  8. Embarque Aereo","  9. Embarque Terrestre","10. Devolución al Productor","11. Traspaso Inter-Planta","12.  M/I Cta. Propia","13. M/I Cta. Productor","14. Muestra Ensayo","15. Botadero","16. Ventas a Terceros","17. Desp. Servicio 3º","20. Packing Externo","21. Venta Exp. País"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Integer(index) = 1 THEN
	ii_tipoi = 2
ELSEIF Integer(index) = 2 THEN
	ii_tipoi = 3
ELSEIF Integer(index) = 3 THEN
	ii_tipoi = 5
ELSEIF Integer(index) = 4 THEN
	ii_tipoi = 6
ELSEIF Integer(index) = 5 THEN
	ii_tipoi = 7
ELSEIF Integer(index) = 6 THEN
	ii_tipoi = 8
ELSEIF Integer(index) = 7 THEN
	ii_tipoi = 9
ELSEIF Integer(index) = 8 THEN
	ii_tipoi = 10
ELSEIF Integer(index) = 9 THEN
	ii_tipoi = 11
ELSEIF Integer(index) = 10 THEN
	ii_tipoi = 12
ELSEIF Integer(index) = 11 THEN
	ii_tipoi = 13
ELSEIF Integer(index) = 12 THEN
	ii_tipoi = 14
ELSEIF Integer(index) = 13 THEN
	ii_tipoi = 15
ELSEIF Integer(index) = 14 THEN
	ii_tipoi = 16
ELSEIF Integer(index) = 15 THEN
	ii_tipoi = 17
ELSEIF Integer(index) = 16 THEN
	ii_tipoi = 20
ELSEIF Integer(index) = 17 THEN
	ii_tipoi = 21
END IF

ddlb_tipoi.PostEvent(Clicked!)
end event

type cbx_4 from checkbox within w_info_despachos_historicos
boolean visible = false
integer x = 1042
integer y = 2092
integer width = 695
integer height = 80
integer taborder = 280
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Despacho Historico"
end type

event clicked;IF This.Checked THEN
	cbx_1.Checked = False
	cbx_2.Checked = False
	cbx_3.Checked = False
	cbx_5.Checked = False
	cbx_6.Enabled = False
	cbx_6.Checked = False
END IF
end event

type cbx_5 from checkbox within w_info_despachos_historicos
boolean visible = false
integer x = 1042
integer y = 2164
integer width = 690
integer height = 76
integer taborder = 290
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desp.Hist.xProductor"
end type

event clicked;IF This.Checked THEN
	cbx_1.Checked = False
	cbx_2.Checked = False
	cbx_3.Checked = False
	cbx_4.Checked = False
	cbx_6.Enabled = False
	cbx_6.Checked = False
END IF
end event

type gb_6 from groupbox within w_info_despachos_historicos
integer x = 1760
integer y = 2048
integer width = 1024
integer height = 200
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Puerto"
end type

type st_6 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 1412
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
string text = "Tipo Camión"
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 1512
integer width = 288
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

type dw_camion from datawindow within w_info_despachos_historicos
integer x = 1019
integer y = 1404
integer width = 987
integer height = 92
integer taborder = 150
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_tipocamion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula
SetNull(li_nula)
iuo_tipocamion = Create uo_tipocamion

IF iuo_tipocamion.existe(Integer(data),True,SqlCa) THEN
	ii_camion	=	integer(data)
ELSE
	This.SetItem(1, "tica_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type dw_embalaje from datawindow within w_info_despachos_historicos
integer x = 1019
integer y = 1500
integer width = 873
integer height = 92
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_embalajesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_nula
SetNull(ls_nula)
iuo_Embalajesprod = Create uo_Embalajesprod

IF iuo_Embalajesprod.existe(gi_CodExport,data,True,SqlCa) THEN
	is_embalajes	=	data
ELSE
	This.SetItem(1, "emba_codigo", ls_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_todoscamion from checkbox within w_info_despachos_historicos
integer x = 2213
integer y = 1404
integer width = 256
integer height = 80
integer taborder = 160
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
boolean righttoleft = true
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_camion = 0
	dw_camion.SetItem(1,"tica_codigo",li_null)
	dw_camion.Object.tica_codigo.Protect	=	1
	dw_camion.Object.tica_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	ii_camion = dw_camion.Object.tica_codigo[1]
	dw_camion.Object.tica_codigo.Protect	=	0
	dw_camion.Object.tica_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_camion.SetFocus()
END IF


end event

type cbx_todosembalaje from checkbox within w_info_despachos_historicos
integer x = 2213
integer y = 1504
integer width = 288
integer height = 80
integer taborder = 180
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
	dw_embalaje.Object.emba_codigo.Protect  =  1
	dw_embalaje.Object.emba_codigo.BackGround.Color  = RGB(192, 192, 192)
ELSE
	is_embalajes = dw_embalaje.Object.emba_codigo[1]
	dw_embalaje.Object.emba_codigo.Protect  =  0
	dw_embalaje.Object.emba_codigo.BackGround.Color  = RGB(255, 255, 255)
	dw_embalaje.SetFocus()
END IF
end event

type cbx_forexcel from checkbox within w_info_despachos_historicos
boolean visible = false
integer x = 3086
integer y = 1320
integer width = 613
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Formato Excel"
boolean checked = true
end type

event clicked;Integer 	li_busca, li_null

SetNull(li_null)


IF cbx_forexcel.Checked = True THEN
	cbx_1.Enabled = False
	cbx_2.Enabled = False
	cbx_3.Enabled = False
	cbx_4.Enabled = False
	cbx_5.Enabled = False
	cbx_1.Checked = False
	cbx_2.Checked = False
	cbx_3.Checked = False
	cbx_4.Checked = False
	cbx_5.Checked = False
	cbx_6.Enabled = False
	cbx_6.Checked = False
	cbx_planta.Visible = True
	cbx_cliente.Visible = True
	cbx_planta.Checked = False
	cbx_cliente.Checked = False
	cbx_todospuerto.Checked = True
	//cbx_conspuerto.Checked = True
	cbx_conspuerto.Enabled = True
	cbx_packing.Enabled = True
	cbx_packingcons.Enabled  = True
ELSE
	cbx_1.Enabled = True
	cbx_2.Enabled = True
	cbx_3.Enabled = True
	cbx_4.Enabled = True
	cbx_5.Enabled = True
	cbx_1.Checked = False
	cbx_2.Checked = False
	cbx_3.Checked = False
	cbx_4.Checked = False
	cbx_5.Checked = False
	cbx_6.Enabled = False
	cbx_6.Checked = False	
	cbx_planta.Visible = False
	cbx_cliente.Visible = False
	cbx_planta.Checked = False
	cbx_cliente.Checked = False
	dw_puerto.Enabled = False
	cbx_todospuerto.Checked = True
	cbx_conspuerto.Checked = True
	cbx_todospuerto.Enabled = False
	cbx_conspuerto.Enabled = False
	cbx_packing.Enabled = False
	cbx_packingcons.Enabled  = False
	dw_packing.Enabled  = False
	
	dw_puerto.SetItem(1,"puer_codigo",li_null)
	dw_puerto.Enabled =False
	
	
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
	
	dw_packing.GetChild("plde_codigo", idwc_packing)
	idwc_packing.SetTransObject(sqlca)
	idwc_packing.Retrieve(2)
	dw_packing.InsertRow(0)
	
END IF	
end event

type cbx_cliente from checkbox within w_info_despachos_historicos
integer x = 2213
integer y = 484
integer width = 311
integer height = 80
integer taborder = 60
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
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	
	IF cbx_todostipop.Checked THEN
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(0,-1)
	ELSE
		dw_productor.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(dw_tipop.Object.tipr_codigo[1],-1)
	END IF	
	
ELSE
	dw_cliente.Object.clie_codigo.Protect	=	0
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_cliente.SetFocus()
END IF
end event

type cbx_planta from checkbox within w_info_despachos_historicos
integer x = 2213
integer y = 576
integer width = 311
integer height = 80
integer taborder = 70
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
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_planta.Object.plde_codigo.Protect	=	0
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_planta.SetFocus()
END IF
end event

type st_16 from statictext within w_info_despachos_historicos
integer x = 1865
integer y = 1736
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

type st_17 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 1736
integer width = 443
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
string text = "No.Guia Desde"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_despachos_historicos
integer x = 251
integer y = 420
integer width = 2592
integer height = 1628
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

type em_guiaini from editmask within w_info_despachos_historicos
integer x = 1019
integer y = 1728
integer width = 402
integer height = 92
integer taborder = 220
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type em_guiafin from editmask within w_info_despachos_historicos
integer x = 2112
integer y = 1728
integer width = 402
integer height = 92
integer taborder = 240
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type dw_puerto from datawindow within w_info_despachos_historicos
integer x = 1783
integer y = 2148
integer width = 1001
integer height = 84
integer taborder = 310
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_puertos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;Return 1
end event

event itemchanged;Integer 	ll_null

SetNull(ll_null)

IF Not iuo_puertos.Existe(Integer(Data), True, sqlca) THEN
	This.SetItem(1, "puer_codigo", Integer(ll_null))

	RETURN 1
END IF		
		


end event

type cbx_todospuerto from checkbox within w_info_despachos_historicos
integer x = 2002
integer y = 2084
integer width = 302
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;Integer li_null
SetNull(li_null)

IF This.Checked THEN
	dw_puerto.SetItem(1,"puer_codigo",li_null)
	dw_puerto.Enabled = False
	dw_puerto.Object.puer_codigo.Protect	=	1
	dw_puerto.Object.puer_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_puerto.Enabled = True
	dw_puerto.Object.puer_codigo.Protect	=	0
	dw_puerto.Object.puer_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_puerto.SetFocus()
END IF
end event

type cbx_conspuerto from checkbox within w_info_despachos_historicos
integer x = 2309
integer y = 2084
integer width = 443
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Consolidado"
end type

event clicked;Integer li_null
SetNull(li_null)

IF This.Checked THEN
	dw_puerto.SetItem(1,"puer_codigo",li_null)
	dw_puerto.Object.puer_codigo.Protect	=	1
	dw_puerto.Object.puer_codigo.BackGround.Color	=	RGB(166,180,210)
	cbx_todospuerto.Checked = True
	cbx_todospuerto.Enabled = False
ELSE
	cbx_todospuerto.Enabled = True
	dw_puerto.Object.puer_codigo.Protect	=	0
	dw_puerto.Object.puer_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_puerto.SetFocus()
END IF
end event

type cbx_6 from checkbox within w_info_despachos_historicos
integer x = 1289
integer y = 1964
integer width = 686
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolida Pallet"
end type

event clicked;IF This.Checked THEN
	cbx_2.Checked = False
	cbx_3.Checked = False
	cbx_4.Checked = False
	cbx_5.Checked = False

END IF
end event

type uo_selespecie from uo_seleccion_especie within w_info_despachos_historicos
event destroy ( )
integer x = 1010
integer y = 652
integer taborder = 120
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cbx_varirotula from checkbox within w_info_despachos_historicos
integer x = 2107
integer y = 1964
integer width = 626
integer height = 72
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

type st_plantadestino from statictext within w_info_despachos_historicos
integer x = 311
integer y = 1148
integer width = 434
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
string text = "Planta Destino"
boolean focusrectangle = false
end type

type dw_plantadestino from datawindow within w_info_despachos_historicos
integer x = 1019
integer y = 1112
integer width = 974
integer height = 92
integer taborder = 110
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
	ii_PlantaDestino		=	Integer(data)
	is_NomPlantaDestino	=	ls_Columna[1]
ELSE
	This.SetItem(1, "plde_codigo", li_Nula)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_7 from checkbox within w_info_despachos_historicos
integer x = 2213
integer y = 1120
integer width = 297
integer height = 80
integer taborder = 90
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

event clicked;Long ll_null
SetNull(ll_null)
IF This.Checked THEN
	ii_PlantaDestino = -1
	dw_plantadestino.Enabled	=	False
	dw_plantadestino.SetItem(1,"plde_codigo",ll_null)
	dw_plantadestino.Object.plde_codigo.Protect	=	1
	dw_plantadestino.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_plantadestino.Enabled	=	True
	dw_plantadestino.Object.plde_codigo.Protect	=	0
	dw_plantadestino.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_plantadestino.SetFocus()
END IF
end event

type st_18 from statictext within w_info_despachos_historicos
integer x = 311
integer y = 1236
integer width = 288
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
string text = "Packing"
boolean focusrectangle = false
end type

type dw_packing from datawindow within w_info_despachos_historicos
integer x = 1019
integer y = 1208
integer width = 969
integer height = 92
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null
Integer	li_Cliente

SetNull(ls_null)

IF ExistePacking(Integer(data)) THEN
	ii_packing	=	Integer(data)
ELSE
	This.SetItem(1, "plde_codigo", integer(ls_null))
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_packing from checkbox within w_info_despachos_historicos
integer x = 2039
integer y = 1212
integer width = 402
integer height = 68
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
	cbx_packingcons.Enabled									=	True
	dw_packing.Enabled										=	False
	dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
	ii_packing = -1
	
	dw_packing.GetChild("plde_codigo", idwc_packing)
	idwc_packing.SetTransObject(sqlca)
	idwc_packing.Retrieve(2)
	dw_packing.InsertRow(0)

ELSE
	cbx_packingcons.Enabled									=	False
	cbx_packingcons.Checked									=	False
	dw_packing.Enabled										=	True
	dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_packing.SetFocus()
END IF
end event

type cbx_packingcons from checkbox within w_info_despachos_historicos
integer x = 2322
integer y = 1208
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	ii_packing = -9
	dw_packing.GetChild("plde_codigo", idwc_packing)
	idwc_packing.SetTransObject(sqlca)
	idwc_packing.Retrieve(2)
	dw_packing.InsertRow(0)
ELSE
	ii_packing = -1
END IF	
end event

type st_19 from statictext within w_info_despachos_historicos
integer x = 251
integer y = 2300
integer width = 2592
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

type st_31 from statictext within w_info_despachos_historicos
integer x = 297
integer y = 2324
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Exportador"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_exportador from datawindow within w_info_despachos_historicos
integer x = 667
integer y = 2316
integer width = 1138
integer height = 96
integer taborder = 260
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_exportadores"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	li_null

SetNull(li_null)

IF existe_exportador(Integer(data)) THEN
	This.SetItem(1, "expo_codigo", li_null)
	ii_exportador = 0
	RETURN 1
ELSE	
	ii_exportador = Integer(data)
END IF	




end event

type cbx_8 from checkbox within w_info_despachos_historicos
integer x = 1774
integer y = 2324
integer width = 192
integer height = 80
integer taborder = 320
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tod."
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_exportador.Enabled		=	False
	dw_exportador.Reset()
	idwc_exportador.Retrieve()
	dw_exportador.InsertRow(0)
	ii_exportador					=	-1
	dw_exportador.Object.expo_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	dw_exportador.Object.expo_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_exportador.Enabled		=	True
END IF
end event

type st_13 from statictext within w_info_despachos_historicos
integer x = 251
integer y = 2048
integer width = 2592
integer height = 252
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

type cbx_tipoi from checkbox within w_info_despachos_historicos
integer x = 2213
integer y = 1020
integer width = 297
integer height = 80
integer taborder = 90
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

event clicked;Long ll_Null
SetNull(ll_Null)

IF This.Checked THEN
	ii_tipoi = -1
	ddlb_tipoi.Enabled = False
ELSE
	ddlb_tipoi.Enabled = True
	ddlb_tipoi.SetFocus()
END IF
end event

