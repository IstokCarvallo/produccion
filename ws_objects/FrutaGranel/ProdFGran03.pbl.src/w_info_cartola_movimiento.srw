$PBExportHeader$w_info_cartola_movimiento.srw
$PBExportComments$Informe Cartola de Movimientos Fruta Granel
forward
global type w_info_cartola_movimiento from w_para_informes
end type
type dw_1 from datawindow within w_info_cartola_movimiento
end type
end forward

global type w_info_cartola_movimiento from w_para_informes
string tag = "Informe Cartola Movimientos"
integer x = 14
integer y = 32
integer width = 3109
integer height = 2352
string title = "Cartola de Movimientos Fruta Granel"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
end type
global w_info_cartola_movimiento w_info_cartola_movimiento

type variables

uo_plantadesp			iuo_plantadesp
uo_camarasfrigo		iuo_camarasfrigo
uo_especie				iuo_especie
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades
uo_tratamientofrio	iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio
uo_categorias			iuo_categorias
uo_productores			iuo_productores
uo_predios           iuo_predios
uo_centrocostos      iuo_centrocosto

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_predio, idwc_ccosto, idwc_tipoenvase, idwc_sentido, &
						idwc_movto, idwc_exporta

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo, ii_productor, ii_predio
end variables

forward prototypes
public function boolean noexistetipoenvase (integer ai_tipo_enva)
end prototypes

public function boolean noexistetipoenvase (integer ai_tipo_enva);String ls_tipo

SELECT	tien_nombre
	INTO	:ls_tipo
	FROM	dba.tiposenvases
	WHERE	enva_tipoen	=	:ai_tipo_enva ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla TiposEnvases")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Tipo Envase (" + String(ai_tipo_enva) + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	
	RETURN TRUE
END IF

RETURN FALSE
end function

on w_info_cartola_movimiento.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_cartola_movimiento.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;call super::open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio		=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores
iuo_predios				=	Create uo_predios
iuo_centrocosto		=	Create uo_centrocostos

//Exportador
dw_1.GetChild("exportador", idwc_exporta)
idwc_exporta.SetTransObject(sqlca)
IF idwc_exporta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Exportador")
	idwc_exporta.InsertRow(0)
END IF

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

//Especie
dw_1.GetChild("especie", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

//Variedad
dw_1.GetChild("variedad",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
IF idwc_variedad.Retrieve(0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Variedades")
	idwc_variedad.InsertRow(0)
END IF

//Grupo
dw_1.GetChild("grupo",idwc_grupo)
idwc_grupo.SetTransObject(Sqlca)
IF idwc_grupo.Retrieve(0,0) = 0 THEN
	MessageBox("Atención","Falta Registrar Grupos")
	idwc_grupo.InsertRow(0)
END IF

//Sub Grupo
dw_1.GetChild("subgrupo",idwc_subgrupo)
idwc_subgrupo.SetTransObject(Sqlca)
IF idwc_subgrupo.Retrieve(0,0) = 0 THEN
	MessageBox("Atención","Falta Registrar Sub Grupos")
	idwc_subgrupo.InsertRow(0)
END IF

//Tratamiento
dw_1.GetChild("tratamiento",idwc_tratamiento)
idwc_tratamiento.SetTransObject(Sqlca)
IF idwc_tratamiento.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tratamientos")
	idwc_tratamiento.InsertRow(0)
END IF

//Periodo
dw_1.GetChild("periodo",idwc_periodo)
idwc_periodo.SetTransObject(Sqlca)
IF idwc_periodo.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Periodos")
	idwc_periodo.InsertRow(0)
END IF

//Categorias
dw_1.GetChild("categoria",idwc_categorias)
idwc_categorias.SetTransObject(Sqlca)
IF idwc_periodo.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Categorias")
	idwc_categorias.InsertRow(0)
END IF
idwc_categorias.Setfilter('isNull( cate_embala ) or  cate_embala <> 1')
idwc_categorias.Filter()

//Productor
dw_1.GetChild("productor",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
IF idwc_productor.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

//Tipo de Envase
dw_1.GetChild("tiposenvase",idwc_tipoenvase)
idwc_tipoenvase.SetTransObject(Sqlca)
IF idwc_tipoenvase.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tipos de Envase")
	idwc_tipoenvase.InsertRow(0)
END IF

//Predio
dw_1.GetChild("predio",idwc_predio)
idwc_predio.SetTransObject(Sqlca)
IF idwc_predio.Retrieve(0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Predios")
	idwc_predio.InsertRow(0)
END IF

//Centro Costo
dw_1.GetChild("centrocosto",idwc_ccosto)
idwc_ccosto.SetTransObject(Sqlca)
IF idwc_ccosto.Retrieve(0,0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Centros de Costo")
	idwc_ccosto.InsertRow(0)
END IF

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fechad", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fechah", Today())
dw_1.Setitem(1,"exportador",gi_codexport)
end event

type pb_excel from w_para_informes`pb_excel within w_info_cartola_movimiento
end type

type st_computador from w_para_informes`st_computador within w_info_cartola_movimiento
end type

type st_usuario from w_para_informes`st_usuario within w_info_cartola_movimiento
end type

type st_temporada from w_para_informes`st_temporada within w_info_cartola_movimiento
end type

type p_logo from w_para_informes`p_logo within w_info_cartola_movimiento
end type

type st_titulo from w_para_informes`st_titulo within w_info_cartola_movimiento
integer x = 256
integer width = 2249
string text = "Informe Cartola de Movimientos de Fruta Granel"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cartola_movimiento
integer x = 2651
integer y = 916
integer taborder = 370
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_Especie, li_Grupo, li_SubGrupo, li_Predio, li_Ccosto, &
			li_Variedad, li_Periodo, li_Categoria, li_tipoenvase, &
			li_ConsLote, li_Consfechad, li_sentidomov, li_movto, li_exporta
Long		ll_Fila, ll_Productor
Date		ld_Fechadesde, ld_FechaHasta, ld_fechaprueba
String	ls_Tratamiento, ls_fecha

istr_info.titulo	= 'CARTOLA DE MOVIMIENTOS FRUTA GRANEL'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_cartola_movimiento"
dw_1.accepttext()
li_Exporta = dw_1.Object.exportador[1]
IF IsNull(li_exporta) Then
	MessageBox( "Exportador Erróneo", "Falta seleccionar un Exportador.", &
				 StopSign!, Ok!)
	RETURN 1				 
END IF

// Acepta Planta //
IF dw_1.Object.todosplanta[1] = 1 THEN
	li_Planta = 10000
ELSE
	li_Planta = dw_1.Object.planta[1]
	IF IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

// Acepta Especie //
IF dw_1.Object.todosespe[1] = 1 THEN
	li_Especie = 100
ELSE
	li_Especie = dw_1.Object.especie[1]
	If IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Variedad //
IF dw_1.Object.todosvari[1] = 1 THEN
	li_Variedad = 10000
ELSE
	li_Variedad = dw_1.Object.variedad[1]
	If IsNull(li_Variedad) Then
		MessageBox( "Variedad Errónea", "Falta seleccionar una Variedad.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Grupo de Variedades //
IF dw_1.Object.todosvari[1] = 1 THEN  
	IF dw_1.Object.todosgrupo[1] = 1  THEN
		li_Grupo = 100
		IF dw_1.Object.consgrupo[1] = 1  THEN	
			li_Grupo =	900
		END IF
	ELSE
		li_Grupo = dw_1.Object.grupo[1]
		If IsNull(li_Grupo) Then
			MessageBox( "Grupo Erróneo", "Falta seleccionar un Grupo de Especie.", &
	      	       StopSign!, Ok!)
			RETURN 1				 
   	END IF
	END IF
ELSE
	li_Grupo = 100
End IF

// Acepta SubGrupo de Variedades //
IF dw_1.Object.todosvari[1] = 1 THEN 
		
		IF dw_1.Object.todossubgru[1] = 1  THEN
			li_SubGrupo = 100
			IF dw_1.Object.conssubgr[1] = 1 THEN
				li_SubGrupo=900
			END IF
		ELSE
			li_SubGrupo = dw_1.Object.subgrupo[1]
			If IsNull(li_SubGrupo) Then
				MessageBox( "SubGrupo Erróneo", "Falta seleccionar un SubGrupo de Especie.", &
	   	          StopSign!, Ok!)
				RETURN 1				 
   		END If
		END IF
	ELSE
		li_SubGrupo = 100
End IF

// Acepta Tratamientos de frio //
IF dw_1.Object.todostrata[1] = 1 THEN
	ls_Tratamiento = '*'
	IF dw_1.Object.constrata[1] = 1 THEN	
		ls_Tratamiento = '**'
	END IF
ELSE
	ls_Tratamiento = dw_1.Object.tratamiento[1]
	If IsNull(ls_Tratamiento) or ls_tratamiento="" Then
		MessageBox( "Tratamiento Erróneo", "Falta seleccionar un Tratamiento.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Periodo de Frio //
IF dw_1.Object.todosperio[1] = 1 THEN
	li_Periodo = 100
	IF dw_1.Object.consperio[1] = 1 THEN	
		li_Periodo = 900
	END IF
ELSE
	li_Periodo = dw_1.Object.periodo[1]
	If IsNull(li_Periodo) Then
		MessageBox( "Período Erróneo", "Falta seleccionar un Período.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Categorias
IF dw_1.Object.todoscate[1] = 1  THEN
	li_Categoria = 1000
ELSE
	li_Categoria = dw_1.Object.categoria[1]
	If IsNull(li_Categoria) Then
		MessageBox( "Categoria Errónea", "Falta seleccionar una Categoria.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Productor
IF dw_1.Object.todosprod[1] = 1 THEN
	ll_Productor = 10000
	IF dw_1.Object.consprod[1] = 1 THEN	
		ll_Productor =	90000
	END IF
ELSE
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Predio
IF dw_1.Object.todospredio[1] = 1 THEN
	li_Predio = 100
	IF dw_1.Object.conspredio[1] = 1 THEN	
		li_Predio =	900
	END IF
ELSE
	li_Predio = dw_1.Object.predio[1]
	If IsNull(li_Predio) Then
		MessageBox( "Predio Erróneo", "Falta seleccionar un Predio.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Centro Costo
IF dw_1.Object.todosccosto[1] = 1 THEN
	li_ccosto = 100
	IF dw_1.Object.conspredio[1] = 1 THEN	
		li_ccosto =	900
	END IF
ELSE
	li_ccosto = dw_1.Object.centrocosto[1]
	If IsNull(li_ccosto) Then
		MessageBox( "Centro Costo Erróneo", "Falta seleccionar un Centro de Costo.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Todos Lotes o Consolida
IF dw_1.Object.consollote[1] = 1 THEN
		li_ConsLote =	1
	ELSE
		li_ConsLote = 0
END IF

// Acepta Tipos de Envase //
IF dw_1.Object.todostiposenvase[1] = 1 THEN
	 li_tipoenvase = 10
ELSE
	 li_tipoenvase = dw_1.Object.tiposenvase[1]
	If IsNull( li_tipoenvase) Then
		MessageBox( "Tipo de Envase Erróneo", "Falta seleccionar un Tipo de Envase.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

ld_fechadesde = dw_1.Object.fechad[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

ls_fecha=mid(String(dw_1.Object.fechah[1],'dd/mm/yyyy'),1,10)
ld_fechahasta = Date(ls_fecha)

If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

IF ld_fechadesde > ld_fechahasta THEN
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1
END IF	

IF dw_1.Object.consfecha[1]= 1 THEN	
	li_consfechad	 =		1
ELSE
	li_Consfechad	 =		0
END IF

IF dw_1.Object.todossenti[1]=1 THEN
	li_sentidomov = 10
ELSE
	li_sentidomov = dw_1.Object.sentido[1]
	IF IsNull(li_sentidomov) Then
		MessageBox( "Sentido Movimiento Erróneo", "Falta seleccionar el Sentido del Movimiento.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

IF dw_1.Object.todosmovto[1]=1 THEN
	li_movto = 1000
	IF dw_1.Object.consmovto[1]=1 THEN	
		li_movto =	9000
	END IF
ELSE
	li_movto = dw_1.Object.movimiento[1]
	IF IsNull(li_movto) Then
		MessageBox( "Movimiento Erróneo", "Falta seleccionar un Movimiento.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Planta,li_Especie,li_Grupo,li_SubGrupo,li_Variedad,ls_Tratamiento, &
                               li_Periodo,ll_Productor, li_predio, li_ccosto, li_Categoria, &
										 li_tipoenvase, li_sentidomov, li_movto, ld_fechadesde, ld_fechahasta, &
										 li_consfechad, li_ConsLote, li_exporta)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
	IF dw_1.Object.totalsubg[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")
	IF dw_1.Object.totalvari[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")
	IF dw_1.Object.totaltrat[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.5.Height=0")
	IF dw_1.Object.totalperi[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.6.Height=0")
	IF dw_1.Object.totalprod[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.7.Height=0")
	IF dw_1.Object.totalmovt[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.8.Height=0")
	
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_cartola_movimiento
integer x = 2651
integer y = 1204
integer taborder = 380
end type

type dw_1 from datawindow within w_info_cartola_movimiento
integer x = 256
integer y = 408
integer width = 2249
integer height = 1724
integer taborder = 380
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_cartola_movto_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;Integer	li_Null, li_cliente
String	ls_Columna

SetNull(li_Null)
dw_1.accepttext()

li_cliente  = dw_1.Object.exportador[1]

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
		
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		END IF		

	CASE "todosplanta"
	  IF data = '1' THEN This.Object.planta[row]		=	li_Null


	CASE "tiposenvase"
		IF noexistetipoenvase(integer(data)) THEN
			This.SetItem(1, "tiposenvase", li_Null )
			This.SetFocus()
			RETURN 1
		END IF	

	CASE "todostiposenvase"
	  IF data = '1' THEN This.Object.tiposenvase[row]	=	li_Null


	CASE "especie"
		This.Object.Variedad[row]	=	li_Null
		This.Object.Grupo[row]		=	li_Null
		This.Object.SubGrupo[row]	=	li_Null
		IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_grupo.settransobject(sqlca)
			idwc_grupo.Retrieve(Integer(data),0)
			idwc_subgrupo.settransobject(sqlca)
			idwc_subgrupo.Retrieve(Integer(data),0)
			idwc_variedad.settransobject(sqlca)
			idwc_variedad.Retrieve(Integer(data))
		END IF
	
  CASE	"todosespe"
		IF	data = '1' THEN
			This.Object.especie[row]		=	li_Null
			this.Object.variedad[row]		=	li_Null
			this.Object.grupo[row]			= 	li_Null
			this.Object.subgrupo[row]		= 	li_Null
			dw_1.Object.todosvari[row]		=	1
			dw_1.Object.todosgrupo[row]	=	1
			dw_1.Object.todossubgru[row]	=	1
		ELSE
			This.Object.especie[row]		=	li_Null
			This.Object.Variedad[row]		=	li_Null
			This.Object.Grupo[row]			=	li_Null
			This.Object.SubGrupo[row]		=	li_Null			
		END IF	
	

	CASE "variedad"
			This.Object.Grupo[row]		=	li_Null
			This.Object.SubGrupo[row]	=	li_Null
			
		IF NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_grupo.settransobject(sqlca)
			idwc_grupo.Retrieve(This.Object.especie[row],Integer(data))
			idwc_subgrupo.settransobject(sqlca)
			idwc_subgrupo.Retrieve(This.Object.especie[row],Integer(data))
			
			This.Object.Grupo[row]			=	iuo_Variedades.Grupo
			This.Object.SubGrupo[row]		=	iuo_Variedades.SubGrupo
			
			dw_1.Object.todosgrupo.protect	=	1
			dw_1.Object.todossubgru.protect	=	1
			dw_1.Object.todosgrupo[1] 			=	0
			dw_1.Object.todossubgru[1] 		=	0
			dw_1.Object.consgrupo[1] 			=	0
			dw_1.Object.conssubgr[1]	 		=	0
			dw_1.Object.consgrupo.protect		=	1
			dw_1.Object.conssubgr.protect		=	1
		END IF
		
	CASE	"todosvari"
		IF	data = '1' THEN
			dw_1.Object.variedad[row]		=	li_Null
			dw_1.Object.grupo[row]			= 	li_Null
			dw_1.Object.subgrupo[row]		= 	li_Null
			dw_1.Object.todosgrupo[1] 		=	1
			dw_1.Object.todossubgru[1] 	=	1
		END IF
		
	CASE "grupo"
		IF NOT iuo_GrupoEspecie.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "grupo", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_subgrupo.Retrieve(This.Object.especie[row],Integer(data))
			This.Object.SubGrupo[row]	=	li_Null
		END IF
			
	CASE "subgrupo"
		IF NOT iuo_SubGrupoEspecie.Existe(This.Object.especie[row],&
					This.Object.grupo[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "subgrupo", li_Null )
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "tratamiento"
		IF NOT iuo_TratamientoFrio.Ofp_Recupera_TratamientoFrio(SqlCa,data,True) THEN
			This.SetItem(1, "tratamiento", String(li_Null))
			This.SetFocus()
			RETURN 1
		END IF

	CASE "periodo"
		IF NOT iuo_PeriodoFrio.Ofp_Recupera_PeriodoFrio(SqlCa,Integer(data),True) THEN
			This.SetItem(1, "periodo", li_Null)
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "categoria"
		IF NOT iuo_Categorias.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "categoria", li_Null)
			This.SetFocus()
			RETURN 1
		END IF

	CASE "productor"
		IF NOT iuo_Productores.Existe(Long(data),True,SqlCa) THEN
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			this.Object.todospredio.protect = 	0
			This.Object.predio[row]			  =	li_Null
			This.Object.centrocosto[row]	  =	li_Null
			idwc_predio.settransobject(sqlca)
			IF idwc_predio.retrieve(integer(data)) = 0 THEN
//			   MessageBox("Atención","Falta Registrar Predios asociados al Productor")
//			   RETURN 1
		   END IF
			
		END IF

	CASE "predio"
		IF NOT iuo_Predios.Existe(SqlCa,True,dw_1.Object.productor[1],Integer(data)) THEN
			This.SetItem(1, "predio", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			this.Object.todosccosto.protect = 	0
			This.Object.centrocosto[row]	  =	li_Null
			IF idwc_ccosto.retrieve(dw_1.Object.productor[1],integer(data)) = 0 THEN
			   MessageBox("Atención","Falta Registrar Predios asociados al Productor-Predio")
			   RETURN 1
		   END IF
			
		END IF
 
   CASE "centrocosto"
		IF NOT iuo_centrocosto.Existe(Sqlca,True,dw_1.Object.productor[1],dw_1.Object.predio[1],Integer(data)) THEN
			This.SetItem(1, "centrocosto", li_Null )
			This.SetFocus()
			RETURN 1
		END IF		
	
	CASE "sentido"
		this.getchild("movimiento",idwc_movto)
		idwc_movto.settransobject(sqlca)
		idwc_movto.retrieve()
		idwc_movto.setfilter("tpmv_sentid = " + data)
		idwc_movto.filter()
	
	CASE	"todossenti"
		IF	data = '1' THEN This.Object.sentido[row]	=	li_Null
	
	CASE	"todosmovto"
		IF	data = '1' THEN This.Object.movimiento[row]	=	li_Null
	
	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[row]	=	String(li_Null)
	
	CASE	"todosperio"
		IF	data = '1' THEN This.Object.periodo[row]	=	li_Null
	
	CASE	"todoscate"
		IF	data = '1' THEN This.Object.categoria[row]	=	li_Null
			
	CASE	"todosprod"
		IF data='0' THEN
			dw_1.Object.todospredio[1]	= 1
			dw_1.Object.todosccosto[1] = 1
			dw_1.Object.todospredio.protect = 1
			dw_1.Object.todosccosto[1] = 1
		ELSEIF	data = '1' THEN 
			This.Object.productor[row]		=	li_Null
			This.Object.predio[row]			=	li_Null
			This.Object.centrocosto[row]	=	li_Null
			dw_1.Object.todospredio.protect = 1
			dw_1.Object.todospredio[row] 	= 1			
		END IF
		
	CASE	"todospredio"
		IF data='0' THEN
			dw_1.Object.todosccosto.protect = 1
		ELSEIF	data = '1' THEN 
			This.Object.predio[row]			=	li_Null
			This.Object.centrocosto[row]	=	li_Null
			dw_1.Object. todosccosto[row]	=  1
		END IF	
	
	CASE	"todosccosto"
		IF	data = '1' THEN This.Object.centrocosto[row]	=	li_Null

END CHOOSE
end event

event itemerror;Return 1
end event

