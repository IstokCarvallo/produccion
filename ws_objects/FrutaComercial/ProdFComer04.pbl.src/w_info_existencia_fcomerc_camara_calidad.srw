$PBExportHeader$w_info_existencia_fcomerc_camara_calidad.srw
$PBExportComments$Ventana Informe de Existencia de Fruta Comercial en Camara.
forward
global type w_info_existencia_fcomerc_camara_calidad from w_para_informes
end type
type dw_1 from datawindow within w_info_existencia_fcomerc_camara_calidad
end type
end forward

global type w_info_existencia_fcomerc_camara_calidad from w_para_informes
integer width = 3118
integer height = 2140
string title = "Existencia Fruta Comercial por Camara"
dw_1 dw_1
end type
global w_info_existencia_fcomerc_camara_calidad w_info_existencia_fcomerc_camara_calidad

type variables
uo_plantadesp			iuo_plantadesp
uo_camarasbode			iuo_camarasbode
uo_especie				iuo_especie
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades
uo_tratamientofrio	iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio
uo_categorias			iuo_categorias
uo_productores			iuo_productores

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_cliente

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo
end variables

event open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_camarasbode		=	Create uo_camarasbode
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio		=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

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
dw_1.GetChild("grupo",idwc_Subgrupo)
idwc_Subgrupo.SetTransObject(Sqlca)
IF idwc_Subgrupo.Retrieve(0,0) = 0 THEN
	MessageBox("Atención","Falta Registrar Grupos")
	idwc_Subgrupo.InsertRow(0)
END IF

//Sub Grupo
dw_1.GetChild("subgrupo",idwc_grupo)
idwc_grupo.SetTransObject(Sqlca)
IF idwc_grupo.Retrieve(0,0) = 0 THEN
	MessageBox("Atención","Falta Registrar Sub Grupos")
	idwc_grupo.InsertRow(0)
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
IF idwc_productor.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

//Exportador
dw_1.GetChild("cliente",idwc_cliente)
idwc_cliente.SetTransObject(Sqlca)
IF idwc_cliente.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Clientes")
	idwc_cliente.InsertRow(0)
END IF

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

dw_1.setitem(1,"cliente",gi_codexport)
dw_1.setitem(1,"tipopool",2)
end event

on w_info_existencia_fcomerc_camara_calidad.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_existencia_fcomerc_camara_calidad.destroy
call super::destroy
destroy(this.dw_1)
end on

type pb_excel from w_para_informes`pb_excel within w_info_existencia_fcomerc_camara_calidad
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_fcomerc_camara_calidad
integer x = 2098
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_fcomerc_camara_calidad
integer x = 2098
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_fcomerc_camara_calidad
integer x = 2098
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_fcomerc_camara_calidad
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_fcomerc_camara_calidad
integer x = 265
integer width = 2373
string text = "Informe Existencia Fruta Comercial por Camara"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_fcomerc_camara_calidad
integer x = 2715
integer y = 728
integer taborder = 320
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Frigorifico, li_Camara, li_Banda, li_Especie, li_Grupo, li_SubGrupo, &
			li_Variedad, li_Periodo, li_Categoria,&
			li_ConsLote, li_cliente, li_tipool, li_Planta
Long		ll_Fila,ll_Productor
String	ls_Tratamiento, ls_Consgcalib

istr_info.titulo	= 'EXISTENCIA DE FRUTA COMERCIAL POR CAMARA'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_existencia_camara01"

// Acepta Planta //
IF dw_1.Object.todosplanta[1] = 1 THEN
	li_Planta = 10000
ELSE
	li_Planta = dw_1.Object.planta[1]
	IF IsNull(li_Planta) Then
		MessageBox( "Frigorifico Erróneo", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

// Acepta Frigorifico //
IF dw_1.Object.todosfrigo[1] = 1 THEN
	li_Frigorifico = 10000
ELSE
	li_Frigorifico = dw_1.Object.frigorifico[1]
	IF IsNull(li_Frigorifico) Then
		MessageBox( "Frigorifico Erróneo", "Falta seleccionar un Frigorífico.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

// Acepta Camara //
IF dw_1.Object.todoscama[1] = 1 THEN
	li_Camara = 10000
	IF dw_1.Object.conscama[1] =1 THEN	
		li_Camara	=	90000
	END IF
ELSE
	li_Camara	=	dw_1.Object.camara[1]
	If IsNull(li_Camara) Then
		MessageBox( "Camara Errónea", "Falta seleccionar una Camara.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

IF dw_1.Object.todasbanda[1]=1 THEN
	li_banda = 100
	IF dw_1.Object.consbanda[1]=1 THEN
		li_Banda  =   900
	END IF
ELSE
	li_Banda = dw_1.Object.banda[1]
	If IsNull(li_Banda) Then
		MessageBox( "Banda Errónea", "Falta seleccionar una Banda.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
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


// Acepta Productor
li_cliente = dw_1.Object.cliente[1]
IF IsNull(li_cliente) Then
	MessageBox( "Cliente Erróneo", "Falta seleccionar un Cliente.", &
             StopSign!, Ok!)
	RETURN 1				 
END If

// Acepta Tipo Pool
li_tipool = dw_1.Object.tipopool[1]

// Acepta Todos Lotes o Consolida
IF dw_1.Object.consollote[1] = 1 THEN
		li_ConsLote =	1
	ELSE
		li_ConsLote = 0
END IF
// Acepta Todos Grupos Calibres ó Consolidados
IF dw_1.Object.consgcalib[1] = 0 THEN
	ls_Consgcalib =	'*'
ELSE
	ls_Consgcalib = '**'
END IF

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Frigorifico,li_Camara,li_banda,li_Especie,li_Grupo,&
										 li_SubGrupo,li_Variedad,ls_Tratamiento,li_Periodo,li_Categoria,&
										 ll_Productor,li_ConsLote,ls_Consgcalib, li_cliente, li_tipool, li_Planta)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)	
	IF dw_1.Object.totalfrig[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.1.Height=0")
	IF dw_1.Object.totalcama[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
	IF dw_1.Object.totalband[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")
	IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")
	IF dw_1.Object.totalgrup[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.5.Height=0")
	IF dw_1.Object.totalsubg[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.6.Height=0")
	IF dw_1.Object.totalvari[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.7.Height=0")
	IF dw_1.Object.totalcate[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.8.Height=0")
	IF dw_1.Object.totaltrat[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.9.Height=0")
	IF dw_1.Object.totalperi[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.10.Height=0")
	IF dw_1.Object.totalgcal[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.11.Height=0")
	IF dw_1.Object.totalprod[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.12.Height=0")
		
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF

END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_fcomerc_camara_calidad
integer x = 2715
integer y = 1088
integer taborder = 330
end type

type dw_1 from datawindow within w_info_existencia_fcomerc_camara_calidad
integer x = 265
integer y = 420
integer width = 2267
integer height = 1412
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_existencia_camarabanda_seleccion"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null
String	ls_Columna

SetNull(li_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		Else
			dw_1.Object.todosfrigo.protect = 0
		END IF		
		
		IF idwc_frigorifico.Retrieve(Integer(data)) = 0 THEN
			MessageBox("Atención","Falta Registrar Frigorífico asociados a Planta Administradora")
			RETURN 1
		ELSE
			idwc_frigorifico.SetFilter("cama_codfri = cama_codigo")
			idwc_frigorifico.Filter()
		END IF
	
		IF idwc_camaras.Retrieve(Integer(data)) = 0 THEN
			MessageBox("Atención","Falta Registrar Cámaras Frigorífico Seleccionado")
			RETURN 1
		END IF
		
	CASE "frigorifico"
		IF NOT iuo_Camarasbode.Existe(This.Object.planta[1],Integer(data),True,Sqlca) THEN
			This.SetItem(1, "frigorifico", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE	
			dw_1.Object.todoscama.protect = 0
		END IF
		
		idwc_camaras.SetFilter("cama_codfri = " + data + " And cama_codfri <> cama_codigo") 
		idwc_camaras.Filter()
						
	CASE "camara"
		IF NOT iuo_Camarasbode.Existe(This.Object.planta[1],Integer(data),True,Sqlca) THEN
			This.SetItem(1, "frigorifico", li_Null )
			This.SetFocus()
			RETURN 1
		ElSE
			dw_1.Object.todoscama.protect = 0
		END IF

	CASE "especie"
		
		IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_grupo.Retrieve(Integer(data),0)
			This.Object.Grupo[1]				=	li_Null
			This.Object.todosgrupo.Protect	=	0
			/**/
			idwc_subgrupo.Retrieve(Integer(data),0)
			This.Object.SubGrupo[1]			=	li_Null
			This.Object.todosSubGru.Protect	=	0			
			/**/
			idwc_variedad.Retrieve(Integer(data),gstr_parempresa.empr_codexp)
			This.Object.Variedad[1]		=	li_Null
		END IF

	CASE "variedad"
		
		IF NOT iuo_Variedades.Existe(This.Object.especie[1],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			This.Object.Grupo[1]			=	iuo_Variedades.Grupo
			This.Object.SubGrupo[1]		=	iuo_Variedades.SubGrupo
			
			This.Object.todosgrupo.Protect	=	1
			This.Object.todossubgru.Protect	=	1
		END IF
		
	CASE "grupo"
		IF NOT iuo_GrupoEspecie.Existe(This.Object.especie[1],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "grupo", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_grupo.Retrieve(This.Object.especie[1],Integer(data))
			This.Object.Grupo[1]	=	li_Null
		END IF
			
	CASE "subgrupo"
		IF NOT iuo_SubGrupoEspecie.Existe(This.Object.especie[1],&
					This.Object.grupo[1],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "subgrupo", li_Null )
			This.SetFocus()
			RETURN 1
		END IF
		MessageBox("",data)
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
		IF NOT iuo_Productores.Existe(long(data),True,SqlCa) THEN
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			RETURN 1
		END IF

	CASE "todosplanta"
		IF data='0' THEN
			dw_1.Object.todosfrigo.protect = 1
		ELSE
		IF data = '1' THEN This.Object.planta[1]		=	li_Null
			dw_1.Object.frigorifico[1]			=	li_Null
			dw_1.Object.todosfrigo[1]			=	1
			dw_1.Object.todosfrigo.Protect 	= 1
			dw_1.Object.camara[1]				=	li_Null
			dw_1.Object.todoscama[1]			=	1
			dw_1.Object.todoscama.Protect 	= 1			
		END IF

	CASE "todosfrigo"
		IF data='0' THEN
			dw_1.Object.todoscama.protect = 0
		ELSE
		IF data = '1' THEN This.Object.frigorifico[1]	=	li_Null
			dw_1.Object.todoscama.protect = 1
			dw_1.Object.camara[1]			=	li_Null
			dw_1.Object.todoscama[1] 		= 1	
		End If

	CASE "todoscama"
		IF data='0' THEN 
			dw_1.Object.todoscama.protect = 1
		ELSE
		IF data = '1' THEN This.Object.camara[1]	= li_Null
			dw_1.Object.camara[1]	=	li_Null
			dw_1.Object.todoscama[1]	=	1
		END IF
		
	CASE	"todosespe"
		IF	data = '1' THEN This.Object.especie[1]	=	li_Null
			dw_1.Object.variedad[1]	= li_Null
			dw_1.Object.todosvari[1]	=	1
			This.SetItem(1, "subgrupo", li_Null )
			This.SetItem(1, "grupo", li_Null )
			
	CASE	"todosvari"
		IF	data = '1' THEN 
			This.Object.variedad[1]	=	li_Null
			dw_1.Object.todosvari[1]	=	1
			This.Object.todosgrupo.Protect	=	0
			This.Object.todossubgru.Protect	=	0
			This.SetItem(1, "subgrupo", li_Null )
			This.SetItem(1, "grupo", li_Null )	
		ELSE	
			This.Object.todosgrupo.Protect	=	1
			This.Object.todossubgru.Protect	=	1
		END IF	
		
	CASE	"todosgrupo"
		IF	data = '1' THEN 
			This.Object.todossubgru.Protect	=	1
			This.SetItem(1, "subgrupo", li_Null )
			This.SetItem(1, "grupo", li_Null )	
		ELSE	
			This.Object.todossubgru.Protect	=	0
		END IF	
				
	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[1]	=	String(li_Null)
	
	CASE	"todosperio"
		IF	data = '1' THEN This.Object.periodo[1]		=	li_Null
	
	CASE	"todoscate"
		IF	data = '1' THEN This.Object.categoria[1]	=	li_Null
			
	CASE	"todosprod"
		IF	data = '1' THEN This.Object.productor[1]	=	li_Null
		RETURN 0
END CHOOSE
end event

event itemerror;RETURN 1
end event

