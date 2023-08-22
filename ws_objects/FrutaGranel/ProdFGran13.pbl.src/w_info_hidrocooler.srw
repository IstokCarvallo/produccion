$PBExportHeader$w_info_hidrocooler.srw
$PBExportComments$Ventana de Informe Existencias de Fruta Granel.
forward
global type w_info_hidrocooler from w_para_informes
end type
type dw_1 from datawindow within w_info_hidrocooler
end type
end forward

global type w_info_hidrocooler from w_para_informes
integer x = 14
integer y = 32
integer width = 2583
integer height = 1900
string title = "Recepciones Por HidroCooler"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
end type
global w_info_hidrocooler w_info_hidrocooler

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
uo_productores    	iuo_productores
uo_ProdPredio			iuo_ProdPredio
uo_ProdCuarteles			iuo_Cuartel
uo_cert_protocolo		iuo_protocolo

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_exporta, idwc_predio, idwc_Cuartel, idwc_condicion, &
						idwc_protocolo

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo, ii_informe
end variables

on w_info_hidrocooler.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_hidrocooler.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;call super::open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_camarasfrigo		=	Create uo_camarasfrigo
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio			=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores
iuo_ProdPredio			=	Create uo_ProdPredio
iuo_Cuartel				=	Create uo_ProdCuarteles
iuo_protocolo			=	Create uo_cert_protocolo

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
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
IF idwc_productor.Retrieve(-1) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

//Predio
dw_1.GetChild("predio",idwc_predio)
idwc_predio.SetTransObject(Sqlca)

//Cuartel
dw_1.GetChild("cuartel", idwc_Cuartel)
idwc_Cuartel.SetTransObject(sqlca)

//Cuartel
dw_1.GetChild("protocolo", idwc_protocolo)
idwc_protocolo.SetTransObject(sqlca)
idwc_protocolo.Retrieve()

//Condicion de mercado
dw_1.GetChild("cmerc",idwc_condicion)
idwc_condicion.SetTransObject(Sqlca)

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

dw_1.Object.cliente[1] = gi_codexport

dw_1.Object.fechainicio[1]	=	RelativeDate(Today(), -365)
dw_1.Object.fechatermi[1]	=	Today()

dw_1.Object.tratesp.protect 	= 	1

end event

type pb_excel from w_para_informes`pb_excel within w_info_hidrocooler
end type

type st_computador from w_para_informes`st_computador within w_info_hidrocooler
end type

type st_usuario from w_para_informes`st_usuario within w_info_hidrocooler
end type

type st_temporada from w_para_informes`st_temporada within w_info_hidrocooler
end type

type p_logo from w_para_informes`p_logo within w_info_hidrocooler
end type

type st_titulo from w_para_informes`st_titulo within w_info_hidrocooler
integer width = 1760
string text = "Recepciones Por Hidrocooler "
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_hidrocooler
integer x = 2130
integer y = 896
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
 
Integer	li_Planta, li_Especie, li_Grupo, li_SubGrupo, li_protocolo, &
			li_Variedad, li_Periodo, li_Categoria, li_NroLote, &
			li_ConsLote,rb_espe,rb_cam,li_Predio, li_tratesp, &
			li_Cuartel, li_condmerc, li_KilosReales, li_condembq, li_hidro
Long		ll_Fila, ll_Productor, ll_Camara, ll_cliente, ll_frigo
String	ls_Tratamiento,ls_titulo
Date		ld_fecini, ld_fecter

istr_info.titulo		= 	"EXISTENCIA DE FRUTA GRANEL"

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = 	"dw_info_hidrocooler"

ll_cliente 				= 	dw_1.Object.cliente[1]

If IsNull(ll_cliente) Then
	MessageBox( "Cliente Erróneo", "Falta Seleccionar un Cliente.", StopSign!, Ok!)
	RETURN 1				 
End If

// Acepta Planta //
If dw_1.Object.todosplanta[1] = 1 Then
	li_Planta = -1
Else
	li_Planta = dw_1.Object.planta[1]
	If IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		RETURN 1				 
   End If
End If

li_KilosReales	=	dw_1.Object.kilosreales[1]

// Acepta FrigorIfico //
If dw_1.Object.TodosFrigo[1] = 1 Then
	ll_Frigo = -1
Else
	ll_Frigo	= dw_1.Object.frigorIfico[1]
	If IsNull(ll_Frigo) Then
		MessageBox( "FrigorIfico Erróneo", "Falta seleccionar un FrigorIfico.", StopSign!, Ok!)
		RETURN 1				 
   End If
End If	

// Acepta Camara //
If dw_1.Object.todoscama[1] = 1 Then
	ll_Camara = -1
Else
	ll_Camara	=	dw_1.Object.camara[1]
	If IsNull(ll_Camara) Then
		MessageBox( "Camara Errónea", "Falta seleccionar una Camara.", StopSign!, Ok!)
		RETURN 1				 
   End If
End If

// Acepta Especie //
If dw_1.Object.todosespe[1] = 1 Then
	li_Especie = -1
Else
	li_Especie = dw_1.Object.especie[1]
	If IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.",  StopSign!, Ok!)
		RETURN 1				 
   End If
End If

// Acepta Variedad //
If dw_1.Object.todosvari[1] = 1 Then
	li_Variedad = -1
Else
	li_Variedad = dw_1.Object.variedad[1]
	If IsNull(li_Variedad) Then
		MessageBox( "Variedad Errónea", "Falta seleccionar una Variedad.", StopSign!, Ok!)
		RETURN 1				 
   End If
End If

// Acepta Grupo de Variedades //
If dw_1.Object.todosvari[1] = 1 Then  
	If dw_1.Object.todosgrupo[1] = 1  Then
		li_Grupo = -1
	Else
		li_Grupo = dw_1.Object.grupo[1]
		If IsNull(li_Grupo) Then
			MessageBox( "Grupo Erróneo", "Falta seleccionar un Grupo de Especie.", StopSign!, Ok!)
			RETURN 1				 
   	End If
	End If
Else
	li_Grupo = -1
End If

// Acepta SubGrupo de Variedades //
If dw_1.Object.todosvari[1] = 1 Then 
		
		If dw_1.Object.todossubgru[1] = 1  Then
			li_SubGrupo = -1
		Else
			li_SubGrupo = dw_1.Object.subgrupo[1]
			If IsNull(li_SubGrupo) Then
				MessageBox( "SubGrupo Erróneo", "Falta seleccionar un SubGrupo de Especie.",  StopSign!, Ok!)
				RETURN 1				 
   		End If
		End If
	Else
		li_SubGrupo = -1
End If
// Acepta Tratamientos de frio //
If dw_1.Object.todostrata[1] = 1 Then
	ls_Tratamiento = '*'
Else
	ls_Tratamiento = dw_1.Object.tratamiento[1]
	If IsNull(ls_Tratamiento) or ls_tratamiento="" Then
		MessageBox( "Tratamiento Erróneo", "Falta seleccionar un Tratamiento.", StopSign!, Ok!)
		RETURN 1				 
   End If
End If
// Acepta Periodo de Frio //
If dw_1.Object.todosperio[1] = 1 Then
	li_Periodo = -1
Else
	li_Periodo = dw_1.Object.periodo[1]
	If IsNull(li_Periodo) Then
		MessageBox( "Período Erróneo", "Falta seleccionar un Período.", StopSign!, Ok!)
		RETURN 1				 
   End If
End If
// Acepta Categorias
If dw_1.Object.todoscate[1] = 1  Then
	li_Categoria = -1
Else
	li_Categoria = dw_1.Object.categoria[1]
	If IsNull(li_Categoria) Then
		MessageBox( "Categoria Errónea", "Falta seleccionar una Categoria.", StopSign!, Ok!)
		RETURN 1				 
   End If
End If
// Acepta Productor
If dw_1.Object.todosprod[1] = 1 Then
	ll_Productor = -1
Else
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.",  StopSign!, Ok!)
		RETURN 1				 
   End If
End If
// Acepta Todos Lotes o Consolida
If dw_1.Object.consollote[1] = 1 Then
	li_ConsLote =	1
Else
	li_ConsLote = 0
End If

// Acepta Predio
If dw_1.Object.todospredio[1] = 1 Then
	li_Predio = -1
Else
	li_Predio = dw_1.Object.predio[1]
	If IsNull(li_Predio) Then
		MessageBox( "Predio Erróneo", "Falta seleccionar un Predio.", &
	             StopSign!, Ok!)
		RETURN				 
   End If
End If

//Acepta Cuartel
If dw_1.Object.todoscuartel[1] = 1 Then
	li_Cuartel = -1
Else
	li_Cuartel  = dw_1.Object.cuartel[1]
	If IsNull(li_Cuartel ) Then
		MessageBox( "Cuartel Erróneo", "Falta seleccionar un Cuartel.", StopSign!, Ok!)
		RETURN				 
   End If
End If

//Acepta condicion de mercado
If dw_1.Object.todoscmerc[1] = 1 Then
	li_condmerc = -1
Else
	li_condmerc  = dw_1.Object.merc[1]
	If IsNull(li_condmerc ) Then
		MessageBox( "Mercado Erróneo", "Falta seleccionar una Cond. de Mercado.", StopSign!, Ok!)
		RETURN				 
   End If
End If

//Acepta condicion de embarque
If dw_1.Object.todosembq[1] = 1 Then
	li_condembq = -1
Else
	li_condembq  = dw_1.Object.embq[1]
	If IsNull(li_condembq ) Then
		MessageBox( "C.Embq Errónea", "Falta seleccionar una Cond. de Embarque.", StopSign!, Ok!)
		RETURN				 
   End If
End If

//Acepta desgarchado
If dw_1.Object.tratesptodos[1] = 1 Then
	li_tratesp = -1
Else
	li_tratesp  = dw_1.Object.tratesp[1]
End If

If dw_1.Object.todosprot[1] = 1 Then
	li_protocolo = -1
Else
	li_protocolo  = dw_1.Object.protocolo[1]
End If

If dw_1.Object.todoshidro[1] = 1 Then
	li_hidro = -1
Else
	li_hidro  = dw_1.Object.hidrocooler[1]
	If IsNull(li_hidro) Then
		MessageBox( "Mercado Erróneo", "Falta seleccionar un estado de Hidrocooler",  StopSign!, Ok!)
		RETURN				 
   End If
End If

ld_fecini	=	dw_1.Object.FechaInicio[1]
ld_fecter	=	dw_1.Object.FechaTermi[1]

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Planta,  li_Especie,	li_Variedad,	ls_Tratamiento, li_Periodo, li_Categoria,&
								ll_Productor, ll_cliente, li_Predio,  li_Cuartel, ld_fecini, ld_fecter, li_hidro)
										 
If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_hidrocooler
integer x = 2139
integer y = 1184
integer taborder = 140
end type

type dw_1 from datawindow within w_info_hidrocooler
integer x = 247
integer y = 412
integer width = 1760
integer height = 1268
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_hidrocooler_seleccion"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged; Integer	li_Null, li_cliente
String	ls_Columna
dw_1.accepttext()
SetNull(li_Null)

li_cliente = dw_1.Object.cliente[1]

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
		
		IF NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			This.Object.Grupo[row]			=	iuo_Variedades.Grupo
			This.Object.SubGrupo[row]		=	iuo_Variedades.SubGrupo
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
			This.Object.predio[row]			  =	li_Null
			idwc_predio.Retrieve(Long(Data))
			idwc_condicion.Reset()
		END IF
		
	CASE "cuartel"
		IF NOT iuo_Cuartel.Existe(This.Object.productor[Row],&
									  This.Object.predio[Row],&
									  Integer(data),True,SQLCA) THEN
		This.Object.cuartel[Row]	=	li_null
		RETURN 1
	ELSE		
		This.Object.Especie[Row]	=	iuo_Cuartel.Especie
		This.Object.variedad[Row]	=	iuo_Cuartel.Variedad
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
		IF	data = '1' THEN This.Object.especie[row]	=	li_Null
			dw_1.Object.variedad[row]	= li_Null
			dw_1.Object.todosvari[row]	=	1

	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[row]	=	String(li_Null)
	
	CASE	"todosperio"
		IF	data = '1' THEN This.Object.periodo[row]		=	li_Null
	
	CASE	"todoscate"
		IF	data = '1' THEN This.Object.categoria[row]	=	li_Null
			
	CASE	"todosprod"
		IF	data = '1' THEN 
			This.Object.productor[row]		=	li_Null
			This.Object.predio[row]		=	li_Null
			This.Object.cmerc[row]			=	li_Null
			This.Object.Todospredio[row]	=	1
			This.Object.TodosCMerc[row]	=	1
		ELSE
//			This.Object.Todospredio[row]	=	0
//			This.Object.TodosCMerc[row]	=	0
		END IF
	
	CASE "predio"
		IF NOT iuo_ProdPredio.Existe(Integer(data),This.Object.productor[Row],&
											  True,SQLCA) THEN
			This.Object.predio[Row]	=	li_null
			RETURN 1
		ELSE		
			idwc_condicion.Retrieve(This.Object.productor[Row],Integer(data),This.Object.especie[row])
			idwc_Cuartel.Retrieve(This.Object.productor[Row],Integer(data))
		END IF
	
	CASE  "consfrigo" 
		IF data = '1' THEN 
			This.Object.conscama[row]  = 1
		ELSE
			This.Object.conscama[row]  = 0
		END IF
		
	CASE "todosfecha"
		dw_1.Object.fechainicio[1]	=	RelativeDate(Today(), -365)
		dw_1.Object.fechatermi[1]	=	Today()
		
	CASE "todosembq"
			This.Object.embq[Row]	=	li_null
		
	CASE "fechainicio"
		IF Date(data) > dw_1.Object.fechatermi[1] THEN
			This.SetItem(1, "fechainicio", RelativeDate(Today(), -365))
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "fechatermino"
		IF Date(data) < dw_1.Object.fechainicio[1] THEN
			This.SetItem(1, "fechatermino", Today())
			This.SetFocus()
			RETURN 1
		END IF
	
	CASE  "tratesptodos" 
		IF data = '1' THEN 
			This.Object.tratesp.protect 	= 	1
			This.Object.tratesp[row]  		= 	0
		ELSE
			This.Object.tratesp.protect 	= 	0
			This.Object.tratesp[row]  		= 	0
		END IF
	
	CASE  "todosprot" 
		This.Object.protocolo[row]  	= 	li_Null
	
	CASE  "protocolo" 
		IF NOT iuo_protocolo.Existe(Integer(data),True,SQLCa) THEN
			This.SetItem(1, "protocolo", li_Null )
			This.SetFocus()
			RETURN 1
		END IF
	
	CASE  "todoshidro" 
		This.Object.hidrocooler[row]  	= 	li_Null
	
	CASE  "hidrocooler" 
		IF data <> '0' AND data <> '1' AND data <> '2' THEN
			MessageBox("Error", "El codigo de Servicio HidroCooler no es valido")
			This.SetItem(1, "hidrocooler", li_Null )
			This.SetFocus()
			RETURN 1
		END IF
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

