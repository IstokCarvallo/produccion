$PBExportHeader$w_info_analisis_controlcalidad.srw
$PBExportComments$Ventana de Informe Existencias de Fruta Granel.
forward
global type w_info_analisis_controlcalidad from w_para_informes
end type
type dw_1 from datawindow within w_info_analisis_controlcalidad
end type
end forward

global type w_info_analisis_controlcalidad from w_para_informes
integer x = 14
integer y = 32
integer width = 2734
integer height = 1700
string title = "Análisis Control de Calidad"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
end type
global w_info_analisis_controlcalidad w_info_analisis_controlcalidad

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

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento,  &
						idwc_productor, idwc_informe, idwc_exportadores

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo
end variables

on w_info_analisis_controlcalidad.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_analisis_controlcalidad.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_camarasfrigo		=	Create uo_camarasfrigo
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio		=	Create uo_tratamientofrio
iuo_productores		=	Create uo_productores

//Informe
dw_1.GetChild("nroinforme", idwc_informe)
idwc_informe.SetTransObject(sqlca)
IF idwc_informe.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Informes")
	idwc_informe.InsertRow(0)
END IF

//exportador
dw_1.GetChild("exportador",idwc_exportadores)
idwc_exportadores.SetTransObject(Sqlca)
idwc_exportadores.Retrieve() 

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

dw_1.Object.planta[1] = gstr_paramplanta.codigoplanta

//Frigorifico
dw_1.GetChild("frigorifico", idwc_frigorifico)
idwc_frigorifico.SetTransObject(sqlca)
idwc_frigorifico.InsertRow(0)

//Camara Frigorifico
dw_1.GetChild("camara", idwc_camaras)
idwc_camaras.SetTransObject(sqlca)
idwc_camaras.InsertRow(0)

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
idwc_variedad.InsertRow(0)

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

//Productor
dw_1.GetChild("productor",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
IF idwc_productor.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF


dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fecrepi", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fecrept", Today())
end event

type st_titulo from w_para_informes`st_titulo within w_info_analisis_controlcalidad
integer x = 46
integer y = 36
integer width = 2194
string text = "Análisis Control de Calidad"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_analisis_controlcalidad
integer x = 2391
integer y = 456
integer height = 140
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_Especie, li_Grupo, li_SubGrupo, li_Variedad, li_tiposelec, li_grupoprod, &
         li_nroinfo,li_detalle
Long		ll_Fila, ll_Productor, ll_Camara, ll_frigo
String	ls_Tratamiento, ls_fecha
Date ld_Fechadesde,   ld_FechaHasta, ld_fechaprueba


istr_info.titulo	= 'ANALISIS DE CONTROL DE CALIDAD'

OpenWithParm(vinf, istr_info)

li_NroInfo = dw_1.Object.nroinforme[1]
IF IsNull(li_nroinfo) Then
	MessageBox( "Tipo de Informe Erróneo", "Falta seleccionar un Tipo de Informe.", &
             StopSign!, Ok!)
	RETURN 1				 
END IF

li_Planta = dw_1.Object.planta[1]
IF IsNull(li_Planta) Then
	MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
				 StopSign!, Ok!)
	RETURN 1				 
END IF

// Acepta Frigorifico //
IF dw_1.Object.todosfrigo[1] = 1 THEN
	ll_Frigo = -1
	IF dw_1.Object.consfrigo[1] = 1 THEN 	ll_Frigo = -9
ELSE
	ll_Frigo	= dw_1.Object.frigorifico[1]
	IF IsNull(ll_Frigo) THEN
		MessageBox( "Frigorifico Erróneo", "Falta seleccionar un Frigorifico.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF	

// Acepta Camara //
IF dw_1.Object.todoscama[1] = 1 THEN
	ll_Camara = -1
	IF dw_1.Object.conscama[1] = 1 THEN	ll_Camara	=	-9
ELSE
	ll_Camara	=	dw_1.Object.camara[1]
	IF IsNull(ll_Camara) THEN
		MessageBox( "Camara Errónea", "Falta seleccionar una Camara.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Especie //
IF dw_1.Object.todosespe[1] = 1 THEN
	li_Especie = -1
ELSE
	li_Especie = dw_1.Object.especie[1]
	IF IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Variedad //
IF dw_1.Object.todosvari[1] = 1 THEN
	li_Variedad = -1
ELSE
	li_Variedad = dw_1.Object.variedad[1]
	If IsNull(li_Variedad) Then
		MessageBox( "Variedad Errónea", "Falta seleccionar una Variedad.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Grupo de Variedades //
IF dw_1.Object.todosgrupo[1] = 1  THEN
	li_Grupo = -1
ELSE
	li_Grupo = dw_1.Object.grupo[1]
	If IsNull(li_Grupo) Then
		MessageBox( "Grupo Erróneo", "Falta seleccionar un Grupo de Especie.", &
					 StopSign!, Ok!)
		RETURN 1				 
		END IF
END IF

// Acepta SubGrupo de Variedades //
IF dw_1.Object.todossubgru[1] = 1  THEN
	li_SubGrupo = -1
ELSE
	li_SubGrupo = dw_1.Object.subgrupo[1]
	If IsNull(li_SubGrupo) Then
		MessageBox( "SubGrupo Erróneo", "Falta seleccionar un SubGrupo de Especie.", &
				 StopSign!, Ok!)
		RETURN 1				 
		END If
END IF

// Acepta Tratamientos de frio //
IF dw_1.Object.todostrata[1] = 1 THEN
	ls_Tratamiento = '*'
ELSE
	ls_Tratamiento = dw_1.Object.tratamiento[1]
	If IsNull(ls_Tratamiento) or ls_tratamiento="" Then
		MessageBox( "Tratamiento Erróneo", "Falta seleccionar un Tratamiento.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Productor
IF dw_1.Object.todosprod[1] = 1 THEN
	ll_Productor = -1
	IF dw_1.Object.consprod[1] = 1 THEN	ll_Productor =	-9
ELSE
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Fecha Desde - Hasta de Recepción
ld_fechadesde = dw_1.Object.fecrepi[1]

If IsNull(ld_fechadesde) or ld_fechadesde <= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

ls_fecha=mid(String(dw_1.Object.fecrept[1],'dd/mm/yyyy'),1,10)
ld_fechahasta = Date(ls_fecha)

If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

IF ld_fechadesde>=ld_fechahasta THEN
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor a la Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1
END IF	

// Tipo Selectivo
li_tiposelec = dw_1.object.tiposelec[1]
li_detalle 	   = dw_1.object.detalle[1]

vinf.dw_1.DataObject = "dw_info_analisiscontrolcalidad_recpro"

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_nroinfo,li_planta, ld_fechadesde, ld_fechahasta, li_especie, &
                               li_grupo, li_subgrupo,li_variedad,ll_productor, li_GrupoProd, &
										 ll_frigo, ll_camara, ls_tratamiento, li_tiposelec)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
	
	vinf.dw_1.GroupCalc()
	
	IF  ll_frigo=-9 THEN 
		vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
		vinf.dw_1.Modify("DataWindow.Header.2.Height=0")
	END IF
	IF ll_camara=-9 THEN 
		vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")
		vinf.dw_1.Modify("DataWindow.Header.3.Height=0")
	END IF
	IF ll_productor=-9 THEN 
		vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")
		vinf.dw_1.Modify("DataWindow.Header.4.Height=0")
		IF li_detalle = 1 THEN
			vinf.dw_1.Modify("DataWindow.Detail.Height=0")
			vinf.dw_1.Modify("DataWindow.Header.5.Height=0")
		END IF

	END IF

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_analisis_controlcalidad
integer x = 2400
integer y = 744
integer height = 140
integer taborder = 140
end type

type dw_1 from datawindow within w_info_analisis_controlcalidad
integer x = 46
integer y = 196
integer width = 2194
integer height = 1308
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_analisiscontrol_selectivo"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null,li_cliente
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
		ElSE
			dw_1.Object.todosfrigo.protect = 0
		END IF
		
		//GetChild Frigorifico
		IF idwc_frigorifico.Retrieve(Integer(data)) = 0 THEN
			MessageBox("Atención", "Falta Registrar las camaras")
			idwc_frigorifico.InsertRow(0)
		ELSE
			idwc_frigorifico.SetFilter("cama_codigo = cama_codfri")
			idwc_frigorifico.Filter()
		END IF

		//GetChild Camara
		IF idwc_camaras.Retrieve(Integer(data)) = 0 THEN
			MessageBox("Atención","Falta Registrar Cámaras Frigorífico Seleccionado")
			idwc_camaras.InsertRow(0)
		END IF
		
	CASE "frigorifico"
		IF Not iuo_Camarasfrigo.Existe(dw_1.Object.Planta[1], Integer(data), True, Sqlca) THEN
			This.SetItem(1, "frigorifico", li_Null )
			This.SetFocus()
			RETURN 1
		ElSE
			dw_1.Object.todoscama.protect = 0
		END IF		

		idwc_camaras.SetFilter("cama_codfri = " + data + " And cama_codfri <> cama_codigo") 
		idwc_camaras.Filter()

	CASE "camara"
		IF NOT iuo_CamarasFrigo.Existe(This.Object.planta[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "camara", li_Null )
			This.SetFocus()
			RETURN 1
		Else
			dw_1.Object.todoscama.protect = 0
		END IF
						
	CASE "especie"
		This.Object.Grupo[row]			=	li_Null
		This.Object.SubGrupo[row]		=	li_Null
      This.Object.variedad[row]     =  li_Null
		IF NOT iuo_especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_grupo.Retrieve(Integer(data),0)
			This.Object.Grupo[row]		=	li_Null
			/**/
			idwc_subgrupo.Retrieve(Integer(data),0)
			This.Object.SubGrupo[row]	=	li_Null
			/**/
			idwc_variedad.Retrieve(Integer(data))
			This.Object.Variedad[row]	=	li_Null

		END IF

	CASE "variedad"
		This.Object.Grupo[row]			=	li_Null
		This.Object.SubGrupo[row]		=	li_Null
		IF NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			This.Object.Grupo[row]			=	iuo_Variedades.Grupo
			this.Object.todosgrupo[row]	=  0
			idwc_subgrupo.Retrieve(This.Object.especie[row],iuo_Variedades.Grupo)
			This.Object.SubGrupo[row]		=	iuo_Variedades.SubGrupo
			this.Object.todossubgru[row]	= 0
		END IF
		
	CASE "grupo"
		This.Object.SubGrupo[row]		=	li_Null
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
		END IF

	CASE "todosplanta"
		IF data='0' THEN
			dw_1.Object.todoscama.protect = 1
		ELSEIF data = '1' THEN 
			This.Object.planta[row]			= li_Null
			dw_1.Object.frigorifico[row]	= li_Null
			dw_1.Object.todosfrigo[row]	= 1
			dw_1.Object.camara[row]			= li_Null
			dw_1.Object.todoscama[row]		= 1
		END IF
		
	CASE "todosfrigo"
		IF data = '0' THEN
			dw_1.Object.todoscama.protect = 1
		ELSEIF data = '1' THEN
			This.Object.frigorifico[row]	= li_Null
			dw_1.Object.camara[row]			= li_Null
			dw_1.Object.todoscama[row]		= 1
		END IF
 
	CASE "todoscama"
		IF data='1' THEN 
			This.Object.camara[row]	= li_Null
			dw_1.Object.camara[row]	=	li_Null
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
	
CASE "todosgrupo"
	IF data = "1" THEN
  		this.Object.grupo[row]			= 	li_Null
		this.Object.subgrupo[row]		= 	li_Null 
		dw_1.Object.todossubgru[row]	=	1
	END IF

CASE "todossubgru"
	IF data = "1" THEN
		this.Object.subgrupo[row]		= 	li_Null 
	END IF
	
  CASE	"todosvari"
		IF	data = '1' THEN
			dw_1.Object.variedad[row]		=	li_Null
			dw_1.Object.grupo[row]			= 	li_Null
			dw_1.Object.subgrupo[row]		= 	li_Null
			dw_1.Object.todosgrupo[1] 		=	1
			dw_1.Object.todossubgru[1] 	=	1
		END IF
		
	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[row]	=	String(li_Null)
	
	CASE	"todosperio"
		IF	data = '1' THEN This.Object.periodo[row]	=	li_Null
	
	CASE	"todoscate"
		IF	data = '1' THEN This.Object.categoria[row]	=	li_Null
			
	CASE	"todosprod"
		IF	data = '1' THEN This.Object.productor[row]	=	li_Null
		
	CASE  "consfrigo" 
		IF data = '1' THEN 
			This.Object.conscama[row]  = 1
		ELSE
			This.Object.conscama[row]  = 0
		END IF
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

event rowfocuschanging;IF newrow > 1 THEN
	This.ScrolltoRow(currentrow)
	RETURN 1
END IF
end event

