$PBExportHeader$w_info_embarque_zonavardestsem_embj.srw
forward
global type w_info_embarque_zonavardestsem_embj from w_para_informes
end type
type st_4 from statictext within w_info_embarque_zonavardestsem_embj
end type
type dw_cliente from datawindow within w_info_embarque_zonavardestsem_embj
end type
type st_6 from statictext within w_info_embarque_zonavardestsem_embj
end type
type st_3 from statictext within w_info_embarque_zonavardestsem_embj
end type
type st_13 from statictext within w_info_embarque_zonavardestsem_embj
end type
type em_semana from editmask within w_info_embarque_zonavardestsem_embj
end type
type st_14 from statictext within w_info_embarque_zonavardestsem_embj
end type
type em_ano from editmask within w_info_embarque_zonavardestsem_embj
end type
type st_10 from statictext within w_info_embarque_zonavardestsem_embj
end type
type dw_recibidor from datawindow within w_info_embarque_zonavardestsem_embj
end type
type cbx_recibidor from checkbox within w_info_embarque_zonavardestsem_embj
end type
type cbx_recibidorcons from checkbox within w_info_embarque_zonavardestsem_embj
end type
type st_1 from statictext within w_info_embarque_zonavardestsem_embj
end type
type cbx_planta from checkbox within w_info_embarque_zonavardestsem_embj
end type
type cbx_plantascons from checkbox within w_info_embarque_zonavardestsem_embj
end type
type dw_planta from datawindow within w_info_embarque_zonavardestsem_embj
end type
type st_8 from statictext within w_info_embarque_zonavardestsem_embj
end type
type cbx_peso from checkbox within w_info_embarque_zonavardestsem_embj
end type
type tit_peso from statictext within w_info_embarque_zonavardestsem_embj
end type
type dw_pesoneto from datawindow within w_info_embarque_zonavardestsem_embj
end type
type gb_3 from groupbox within w_info_embarque_zonavardestsem_embj
end type
type st_5 from statictext within w_info_embarque_zonavardestsem_embj
end type
type st_7 from statictext within w_info_embarque_zonavardestsem_embj
end type
type st_11 from statictext within w_info_embarque_zonavardestsem_embj
end type
type cbx_mercado from checkbox within w_info_embarque_zonavardestsem_embj
end type
type dw_mercado from datawindow within w_info_embarque_zonavardestsem_embj
end type
type st_12 from statictext within w_info_embarque_zonavardestsem_embj
end type
type dw_zonas from datawindow within w_info_embarque_zonavardestsem_embj
end type
type cbx_zonas from checkbox within w_info_embarque_zonavardestsem_embj
end type
type cbx_zonascons from checkbox within w_info_embarque_zonavardestsem_embj
end type
type st_variedad from statictext within w_info_embarque_zonavardestsem_embj
end type
type cbx_merccons from checkbox within w_info_embarque_zonavardestsem_embj
end type
type st_envase from statictext within w_info_embarque_zonavardestsem_embj
end type
type cbx_envase from checkbox within w_info_embarque_zonavardestsem_embj
end type
type uo_selespecie from uo_seleccion_especie within w_info_embarque_zonavardestsem_embj
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_embarque_zonavardestsem_embj
end type
type cbx_varirotula from checkbox within w_info_embarque_zonavardestsem_embj
end type
type cbx_flete from checkbox within w_info_embarque_zonavardestsem_embj
end type
type st_2 from statictext within w_info_embarque_zonavardestsem_embj
end type
type st_16 from statictext within w_info_embarque_zonavardestsem_embj
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_embarque_zonavardestsem_embj
end type
end forward

global type w_info_embarque_zonavardestsem_embj from w_para_informes
integer x = 14
integer y = 32
integer width = 3890
integer height = 2024
string title = "Embarque Zonas/Variedad/Destinos/Semanas"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
dw_cliente dw_cliente
st_6 st_6
st_3 st_3
st_13 st_13
em_semana em_semana
st_14 st_14
em_ano em_ano
st_10 st_10
dw_recibidor dw_recibidor
cbx_recibidor cbx_recibidor
cbx_recibidorcons cbx_recibidorcons
st_1 st_1
cbx_planta cbx_planta
cbx_plantascons cbx_plantascons
dw_planta dw_planta
st_8 st_8
cbx_peso cbx_peso
tit_peso tit_peso
dw_pesoneto dw_pesoneto
gb_3 gb_3
st_5 st_5
st_7 st_7
st_11 st_11
cbx_mercado cbx_mercado
dw_mercado dw_mercado
st_12 st_12
dw_zonas dw_zonas
cbx_zonas cbx_zonas
cbx_zonascons cbx_zonascons
st_variedad st_variedad
cbx_merccons cbx_merccons
st_envase st_envase
cbx_envase cbx_envase
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
cbx_flete cbx_flete
st_2 st_2
st_16 st_16
uo_selproductor uo_selproductor
end type
global w_info_embarque_zonavardestsem_embj w_info_embarque_zonavardestsem_embj

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, &
						idwc_mercado, idwc_productor, idwc_packing, &
						idwc_pesoneto, idwc_recibidor, idwc_zonas
						
						
uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad	
uo_seleccion_varios_productores	iuo_selproductor

String is_NomPlanta
end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean existepacking (integer li_planta)
public function boolean existeproductor (long ll_productor)
end prototypes

public function boolean existeespecie (integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dba.especies
	WHERE	espe_codigo	=	:Especie ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Especies")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
WHERE	 plde_codigo =  :li_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[7] = String(li_planta)
	RETURN True 
END IF
end function

public function boolean existeproductor (long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:ll_Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_embarque_zonavardestsem_embj.create
int iCurrent
call super::create
this.st_4=create st_4
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_3=create st_3
this.st_13=create st_13
this.em_semana=create em_semana
this.st_14=create st_14
this.em_ano=create em_ano
this.st_10=create st_10
this.dw_recibidor=create dw_recibidor
this.cbx_recibidor=create cbx_recibidor
this.cbx_recibidorcons=create cbx_recibidorcons
this.st_1=create st_1
this.cbx_planta=create cbx_planta
this.cbx_plantascons=create cbx_plantascons
this.dw_planta=create dw_planta
this.st_8=create st_8
this.cbx_peso=create cbx_peso
this.tit_peso=create tit_peso
this.dw_pesoneto=create dw_pesoneto
this.gb_3=create gb_3
this.st_5=create st_5
this.st_7=create st_7
this.st_11=create st_11
this.cbx_mercado=create cbx_mercado
this.dw_mercado=create dw_mercado
this.st_12=create st_12
this.dw_zonas=create dw_zonas
this.cbx_zonas=create cbx_zonas
this.cbx_zonascons=create cbx_zonascons
this.st_variedad=create st_variedad
this.cbx_merccons=create cbx_merccons
this.st_envase=create st_envase
this.cbx_envase=create cbx_envase
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.cbx_flete=create cbx_flete
this.st_2=create st_2
this.st_16=create st_16
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_13
this.Control[iCurrent+6]=this.em_semana
this.Control[iCurrent+7]=this.st_14
this.Control[iCurrent+8]=this.em_ano
this.Control[iCurrent+9]=this.st_10
this.Control[iCurrent+10]=this.dw_recibidor
this.Control[iCurrent+11]=this.cbx_recibidor
this.Control[iCurrent+12]=this.cbx_recibidorcons
this.Control[iCurrent+13]=this.st_1
this.Control[iCurrent+14]=this.cbx_planta
this.Control[iCurrent+15]=this.cbx_plantascons
this.Control[iCurrent+16]=this.dw_planta
this.Control[iCurrent+17]=this.st_8
this.Control[iCurrent+18]=this.cbx_peso
this.Control[iCurrent+19]=this.tit_peso
this.Control[iCurrent+20]=this.dw_pesoneto
this.Control[iCurrent+21]=this.gb_3
this.Control[iCurrent+22]=this.st_5
this.Control[iCurrent+23]=this.st_7
this.Control[iCurrent+24]=this.st_11
this.Control[iCurrent+25]=this.cbx_mercado
this.Control[iCurrent+26]=this.dw_mercado
this.Control[iCurrent+27]=this.st_12
this.Control[iCurrent+28]=this.dw_zonas
this.Control[iCurrent+29]=this.cbx_zonas
this.Control[iCurrent+30]=this.cbx_zonascons
this.Control[iCurrent+31]=this.st_variedad
this.Control[iCurrent+32]=this.cbx_merccons
this.Control[iCurrent+33]=this.st_envase
this.Control[iCurrent+34]=this.cbx_envase
this.Control[iCurrent+35]=this.uo_selespecie
this.Control[iCurrent+36]=this.uo_selvariedad
this.Control[iCurrent+37]=this.cbx_varirotula
this.Control[iCurrent+38]=this.cbx_flete
this.Control[iCurrent+39]=this.st_2
this.Control[iCurrent+40]=this.st_16
this.Control[iCurrent+41]=this.uo_selproductor
end on

on w_info_embarque_zonavardestsem_embj.destroy
call super::destroy
destroy(this.st_4)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_13)
destroy(this.em_semana)
destroy(this.st_14)
destroy(this.em_ano)
destroy(this.st_10)
destroy(this.dw_recibidor)
destroy(this.cbx_recibidor)
destroy(this.cbx_recibidorcons)
destroy(this.st_1)
destroy(this.cbx_planta)
destroy(this.cbx_plantascons)
destroy(this.dw_planta)
destroy(this.st_8)
destroy(this.cbx_peso)
destroy(this.tit_peso)
destroy(this.dw_pesoneto)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.st_11)
destroy(this.cbx_mercado)
destroy(this.dw_mercado)
destroy(this.st_12)
destroy(this.dw_zonas)
destroy(this.cbx_zonas)
destroy(this.cbx_zonascons)
destroy(this.st_variedad)
destroy(this.cbx_merccons)
destroy(this.st_envase)
destroy(this.cbx_envase)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.cbx_flete)
destroy(this.st_2)
destroy(this.st_16)
destroy(this.uo_selproductor)
end on

event open;Boolean lb_Cerrar

x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

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
//	uo_selvariedad.Enabled		=	False	
END IF

// uo_seleccion_productor
IF IsNull(uo_selproductor.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	//Close(This)
	lb_Cerrar = False
ELSE
	uo_selproductor.Seleccion(True,True)
END IF

dw_recibidor.GetChild("reci_codigo", idwc_recibidor)
idwc_recibidor.SetTransObject(sqlca)
idwc_recibidor.Retrieve()
dw_recibidor.InsertRow(0)

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve()
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 8.20)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

dw_zonas.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(sqlca)
idwc_zonas.Retrieve()
dw_zonas.InsertRow(0)
dw_zonas.SetItem(1, "zona_codigo", 2)

dw_mercado.GetChild("merc_codigo", idwc_mercado)
idwc_mercado.SetTransObject(sqlca)
idwc_mercado.Retrieve()
dw_mercado.InsertRow(0)

IF Month(Today()) <= 10 THEN
	em_ano.Text					=	String(year(Today()) - 1)
ELSE
	em_ano.Text					=	String(year(Today()))
END IF

em_semana.Text				=	"45"
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	"0"							//	planta
//istr_mant.argumento[5]	=	String(gi_CodEspecie)	//	especie
//istr_mant.argumento[6]  =  "0"							//	productor
istr_mant.argumento[8]  =  "1"							//	peso
istr_mant.argumento[9]  =  em_semana.text				//	semana inicial
istr_mant.argumento[10] =  em_ano.text					//	año inicio temporada
//istr_mant.argumento[11]	=	"1"							// Consolida Productor 1 = SI
istr_mant.argumento[12]	=	"1"							// Consolida Plantas   1 = Si	
istr_mant.argumento[13]	=	"0"							// recibidor	
istr_mant.argumento[14]	=	"1"							// Consolida recibidor 1 = Si
istr_mant.argumento[15]	=	"0"							// Mercado
istr_mant.argumento[16]	=	"1"							// Consolida zonas     1 = Si
istr_mant.argumento[17]	=	"0"							// zonas
//istr_mant.argumento[18]	=	"0"							// variedad
//istr_mant.argumento[19]	=	"1"							// Consolida variedad  1 = Si
istr_mant.argumento[20]	=	"1"							// Consolida envase    1 = Si
istr_mant.argumento[21]	=	"1"							// Consolida mercado   1 = Si

dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
dw_zonas.Object.zona_codigo.BackGround.Color	=	RGB(166,180,210)
dw_mercado.Object.merc_codigo.BackGround.Color	=	RGB(166,180,210)
dw_recibidor.Object.reci_codigo.BackGround.Color	=	RGB(166,180,210)

end event

type st_computador from w_para_informes`st_computador within w_info_embarque_zonavardestsem_embj
end type

type st_usuario from w_para_informes`st_usuario within w_info_embarque_zonavardestsem_embj
end type

type st_temporada from w_para_informes`st_temporada within w_info_embarque_zonavardestsem_embj
end type

type p_logo from w_para_informes`p_logo within w_info_embarque_zonavardestsem_embj
end type

type st_titulo from w_para_informes`st_titulo within w_info_embarque_zonavardestsem_embj
integer width = 3122
string text = "Embarque Zonas/Variedad/Destinos/Semanas/Embalajes"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_embarque_zonavardestsem_embj
integer x = 3483
integer y = 1208
integer taborder = 260
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	ll_Fila, li_cliente, li_planta, li_semana, li_NroSemana, &
			li_tipo = 1, li_zonas,li_semanatope, li_varirotula
Long		ll_semana_ano, ll_productor,ll_semanatope
Date		ld_desde, ld_hasta, ld_FechaInicio, ld_Fecha,ld_FechaPrincipal
String	ls_cajas, ls_especie, ls_planta, ls_productor, ls_encabezado, ls_variedad, ls_zonas, ls_lista

istr_info.titulo	= 'EMBARQUES POR ZONAS/VARIEDAD/DESTINO/SEMANAS/EMBALAJES'

OpenWithParm(vinf, istr_info)

IF cbx_flete.Checked THEN
	vinf.dw_1.DataObject = "dw_info_embarque_semanal_02_embjflete"
ELSE	
	vinf.dw_1.DataObject = "dw_info_embarque_semanal_02_embj"
END IF
	
li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
ll_productor	=	Long(istr_mant.argumento[6])
li_zonas			=	Integer(istr_mant.argumento[17])
li_semana		=	Integer(istr_mant.argumento[9])
ll_semana_ano	=	Integer(istr_mant.argumento[10]) * 100 + li_Semana

IF cbx_peso.Checked=False THEN
	ls_cajas = "Bulto"
	istr_mant.argumento[8]	=	"1"
ELSE
	istr_mant.argumento[8]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas = "Base " + istr_mant.argumento[8] 
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

DECLARE InicioSemana PROCEDURE FOR dba.FProc_InicioSemana
		@semana 		= 	:ll_Semana_Ano,
		@tipo   		= 	:li_tipo  ;
EXECUTE InicioSemana;

IF SQLCA.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Asignacion Inicio de Semana")
	RETURN
ELSEIF SQLCA.SQLCode = 0 THEN
	
	FETCH InicioSemana INTO :ld_FechaInicio ;
	
	IF SQLCA.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Asignacion Inicio de Semana")
	ELSE
		CLOSE InicioSemana	;
	END IF
END IF

ld_Fecha	=	ld_FechaInicio

IF cbx_plantascons.checked THEN
	ls_planta = 'Consolidadas'
ELSE
	IF cbx_planta.checked THEN
		ls_planta = 'Todas'
	ELSE
		SELECT plde_nombre INTO:ls_planta
		FROM DBA.plantadesp
		WHERE plde_codigo=:li_planta;
		ls_planta = String(li_planta,'00')+" "+ls_planta
	END IF
END IF

/*
productor
*/
ls_lista = uo_selproductor.Lista

IF ls_lista = '-9' THEN
	ls_productor = 'Consolidados'
ELSE
	IF ls_lista = '-1' THEN
		ls_productor = 'Todos'
	ELSE
		SELECT prod_nombre INTO:ls_productor
		FROM DBA.productores
		WHERE prod_codigo=:ls_lista;
		ls_productor = String(ls_lista)+" "+ls_productor
		
		IF ls_productor = '00000' THEN
			ls_productor = ls_lista
		END IF	
	END IF
END IF

IF cbx_zonascons.checked THEN
	ls_zonas = 'Consolidadas'
ELSE
	IF cbx_zonas.checked THEN
		ls_zonas = 'Todas'
	ELSE
		SELECT zona_nombre INTO:ls_zonas
		FROM DBA.zonas
		WHERE zona_codigo=:li_zonas;
		ls_zonas = String(li_zonas,'00')+" "+ls_zonas
	END IF
END IF

ls_especie = String(uo_selEspecie.Codigo,'00')+" "+uo_selEspecie.Nombre
ls_variedad = String(uo_selvariedad.Codigo,'00')+" "+uo_selvariedad.Nombre

ld_FechaPrincipal	=	gd_fecultsemana//Date(em_ano.Text+'-12-31')	//

SELECT dba.F_Semana(:ld_FechaPrincipal, 1) 
INTO :ll_semanatope
FROM dba.parempresa;
li_semanatope	=	Integer(Right(String(ll_semanatope),2))

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, uo_selEspecie.Codigo, Dec(istr_mant.argumento[8]),&
										 Integer(istr_mant.argumento[14]),Integer(istr_mant.argumento[12]),&
										 li_semana,ll_semana_ano,Long(istr_mant.argumento[13]),&
										 Integer(istr_mant.argumento[15]),Integer(istr_mant.argumento[16]),&
										 Integer(istr_mant.argumento[17]), uo_selVariedad.Codigo,&
										 Integer(istr_mant.argumento[20]),Integer(istr_mant.argumento[21]),&
										 li_varirotula,ls_lista)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		FOR li_NroSemana = 1 TO li_semanatope
			IF li_NroSemana < 27 THEN
				ls_encabezado	=  "Semana" + String(li_NroSemana, '00') + "_t.Text = '" + &
										String(ld_Fecha, 'dd/mm/yyyy') + "~n~r" + &
										String(li_Semana, '00') + "'"
				vinf.dw_1.Modify(ls_encabezado)
			ELSE
				ls_encabezado	=	"Semana" + String(li_NroSemana, '00') + "_t.Text = '" + &
										String(li_Semana, '00') + "~n~r" + &
										String(ld_Fecha, 'dd/mm/yyyy') + "'"
				vinf.dw_1.Modify(ls_encabezado)
			END IF
			
			ld_Fecha		=	RelativeDate(ld_Fecha, 7)
			li_Semana	++
			
			IF li_Semana = li_semanatope + 1 THEN
				li_Semana	=	1	
			END IF
		NEXT
		vinf.dw_1.Modify("base.text = '" + ls_cajas + "'")
		vinf.dw_1.Modify("tit_especie.text = '" + ls_especie + "'")
		vinf.dw_1.Modify("tit_productor.text = '" + ls_productor + "'")
		vinf.dw_1.Modify("tit_planta.text = '" + ls_planta + "'")
		vinf.dw_1.Modify("tit_variedad.text = '" + ls_variedad + "'")
		vinf.dw_1.Modify("tit_zonas.text = '" + ls_zonas + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_embarque_zonavardestsem_embj
integer x = 3479
integer y = 1492
integer taborder = 270
end type

type st_4 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 251
integer y = 440
integer width = 1573
integer height = 856
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

type dw_cliente from datawindow within w_info_embarque_zonavardestsem_embj
integer x = 626
integer y = 472
integer width = 1143
integer height = 92
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
	
	uo_selproductor.Filtra(-1,-1,Integer(data))
	
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 302
integer y = 480
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 302
integer y = 1396
integer width = 421
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

type st_13 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 1874
integer y = 552
integer width = 434
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Semana Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_semana from editmask within w_info_embarque_zonavardestsem_embj
integer x = 2629
integer y = 520
integer width = 251
integer height = 92
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "45"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "00"
boolean spin = true
string displaydata = "~t/"
double increment = 1
string minmax = "1~~52"
end type

event modified;istr_mant.argumento[9]  =  This.text	
end event

type st_14 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 1870
integer y = 684
integer width = 773
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Año Inicio de Temporada"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_ano from editmask within w_info_embarque_zonavardestsem_embj
integer x = 2624
integer y = 660
integer width = 334
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "0000"
boolean spin = true
string displaydata = "~t/"
double increment = 1
string minmax = "~~"
end type

event em_ano::modified;call super::modified;istr_mant.argumento[10]  =  This.text	
end event

type st_10 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 1851
integer y = 1516
integer width = 416
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
string text = "Consignatario"
boolean focusrectangle = false
end type

type dw_recibidor from datawindow within w_info_embarque_zonavardestsem_embj
integer x = 2254
integer y = 1504
integer width = 1125
integer height = 92
integer taborder = 250
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_consignatarios"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[13]	=	data
istr_mant.argumento[14]	=	'0'

end event

event itemerror;RETURN 1
end event

type cbx_recibidor from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 2254
integer y = 1420
integer width = 402
integer height = 72
integer taborder = 220
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
	cbx_recibidorcons.Enabled									=	True
	dw_recibidor.Enabled											=	False
	dw_recibidor.Object.reci_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[13]									=	'0'
	istr_mant.argumento[14]									=	'0'
ELSE
	cbx_recibidorcons.Enabled									=	False
	cbx_recibidorcons.Checked									=	False
	dw_recibidor.Enabled											=	True
	dw_recibidor.Object.reci_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_recibidor.SetFocus()
END IF

	
end event

type cbx_recibidorcons from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 2679
integer y = 1420
integer width = 471
integer height = 72
integer taborder = 230
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[14]	=	'1'
ELSE
	istr_mant.argumento[14]	=	'0'
END IF
	
end event

type st_1 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 302
integer y = 652
integer width = 462
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
string text = "Planta"
boolean focusrectangle = false
end type

type cbx_planta from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 626
integer y = 580
integer width = 402
integer height = 76
integer taborder = 20
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
	cbx_plantascons.Enabled									=	True
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[2]									=	'0'
	istr_mant.argumento[12]									=	'0'
ELSE
	cbx_plantascons.Enabled									=	False
	cbx_plantascons.Checked									=	False
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)

	dw_planta.SetFocus()
END IF
end event

type cbx_plantascons from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 1102
integer y = 580
integer width = 471
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
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[12]	=	'1'
ELSE
	istr_mant.argumento[12]	=	'0'
END IF
	
end event

type dw_planta from datawindow within w_info_embarque_zonavardestsem_embj
integer x = 626
integer y = 664
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
	istr_mant.argumento[12]	=  "0"
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_8 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 302
integer y = 1144
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

type cbx_peso from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 2295
integer y = 892
integer width = 631
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cajas Equivalentes"
end type

event clicked;IF This.Checked THEN
	tit_peso.Enabled		=	True
	dw_pesoneto.Enabled	=	True
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(255,255,255)))

ELSE
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
END IF

end event

type tit_peso from statictext within w_info_embarque_zonavardestsem_embj
integer x = 2066
integer y = 1004
integer width = 160
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_pesoneto from datawindow within w_info_embarque_zonavardestsem_embj
integer x = 2299
integer y = 992
integer width = 695
integer height = 92
integer taborder = 190
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type gb_3 from groupbox within w_info_embarque_zonavardestsem_embj
integer x = 1888
integer y = 888
integer width = 1417
integer height = 240
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_5 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 251
integer y = 1296
integer width = 1573
integer height = 460
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

type st_7 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 1824
integer y = 440
integer width = 1545
integer height = 712
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

type st_11 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 1851
integer y = 1260
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
string text = "Mercado"
boolean focusrectangle = false
end type

type cbx_mercado from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 2254
integer y = 1188
integer width = 402
integer height = 68
integer taborder = 200
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
	dw_mercado.Enabled										=	False
	dw_mercado.Object.merc_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[15]									=	'0'
ELSE
	dw_mercado.Enabled										=	True
	dw_mercado.Object.merc_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_mercado.SetFocus()
END IF

	
end event

type dw_mercado from datawindow within w_info_embarque_zonavardestsem_embj
integer x = 2254
integer y = 1256
integer width = 887
integer height = 92
integer taborder = 210
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_mercado"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[15]	=	data

end event

event itemerror;RETURN 1
end event

type st_12 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 302
integer y = 860
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
string text = "Zona"
boolean focusrectangle = false
end type

type dw_zonas from datawindow within w_info_embarque_zonavardestsem_embj
integer x = 626
integer y = 852
integer width = 873
integer height = 92
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[17]	=	data

end event

event itemerror;RETURN 1
end event

type cbx_zonas from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 626
integer y = 772
integer width = 402
integer height = 80
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

event clicked;IF This.Checked THEN
	cbx_zonascons.Enabled									=	True
	dw_zonas.Enabled											=	False
	dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(166,180,210)
	istr_mant.argumento[17]									=	'0'
	istr_mant.argumento[16]									=	'0'
ELSE
	cbx_zonascons.Enabled									=	False
	cbx_zonascons.Checked									=	False
	dw_zonas.Enabled											=	True
	dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(255, 255, 255)
	dw_zonas.SetFocus()
END IF
end event

type cbx_zonascons from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 1102
integer y = 772
integer width = 471
integer height = 80
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'1'
ELSE
	istr_mant.argumento[16]	=	'0'
END IF
	
end event

type st_variedad from statictext within w_info_embarque_zonavardestsem_embj
integer x = 302
integer y = 1576
integer width = 279
integer height = 96
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

type cbx_merccons from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 2679
integer y = 1188
integer width = 471
integer height = 68
integer taborder = 240
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[21]	=	'1'
ELSE
	istr_mant.argumento[21]	=	'0'
END IF
end event

type st_envase from statictext within w_info_embarque_zonavardestsem_embj
integer x = 1870
integer y = 804
integer width = 526
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_envase from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 2624
integer y = 800
integer width = 594
integer height = 80
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[20]		=	'1'
ELSE
	istr_mant.argumento[20]		=	'0'
END IF

end event

type uo_selespecie from uo_seleccion_especie within w_info_embarque_zonavardestsem_embj
event destroy ( )
integer x = 626
integer y = 1312
integer height = 180
integer taborder = 260
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
	//	uo_selvariedad.Filtra(This.Codigo)
	//	uo_selvariedad.Enabled						=	False		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
		uo_selvariedad.Enabled						=	True	
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_embarque_zonavardestsem_embj
event destroy ( )
integer x = 626
integer y = 1488
integer taborder = 280
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

event ue_cambio;call super::ue_cambio;IF uo_selespecie.codigo = -1 OR uo_selespecie.codigo = -9 THEN
	uo_selvariedad.dw_seleccion.Enabled = False
ELSE
	uo_selvariedad.dw_seleccion.Enabled = True
END IF		
end event

type cbx_varirotula from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 626
integer y = 1668
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

type cbx_flete from checkbox within w_info_embarque_zonavardestsem_embj
integer x = 2354
integer y = 1660
integer width = 631
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Por Tipo Flete"
end type

event clicked;IF This.Checked THEN
	tit_peso.Enabled		=	True
	dw_pesoneto.Enabled	=	True
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(255,255,255)))

ELSE
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
END IF

end event

type st_2 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 1829
integer y = 1640
integer width = 1545
integer height = 116
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

type st_16 from statictext within w_info_embarque_zonavardestsem_embj
integer x = 1829
integer y = 1152
integer width = 1545
integer height = 488
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

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_embarque_zonavardestsem_embj
integer x = 631
integer y = 968
integer taborder = 190
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

