$PBExportHeader$w_info_desverdizado_lotes.srw
$PBExportComments$Ventana de Informe Existencias de Fruta Granel.
forward
global type w_info_desverdizado_lotes from w_para_informes
end type
type dw_1 from datawindow within w_info_desverdizado_lotes
end type
end forward

global type w_info_desverdizado_lotes from w_para_informes
integer x = 14
integer y = 32
integer width = 2606
integer height = 1940
string title = "DESVERDIZADO DE LOTES"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
end type
global w_info_desverdizado_lotes w_info_desverdizado_lotes

type variables
uo_plantadesp			iuo_plantadesp
uo_camarasfrigo		iuo_camarasfrigo
uo_especie				iuo_especie
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades
uo_tratamientofrio		iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio
uo_categorias			iuo_categorias
uo_productores   	 	iuo_productores
uo_ProdPredio			iuo_ProdPredio
uo_ProdCuarteles			iuo_Cuartel
uo_cert_protocolo		iuo_protocolo
uo_Clientesprod			iuo_Cliente

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_exporta, idwc_predio, idwc_Cuartel, idwc_condicion, &
						idwc_protocolo

String		is_informe
Integer	ii_informe

end variables

on w_info_desverdizado_lotes.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_desverdizado_lotes.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;call super::open;x	=	0
y	=	0

ii_informe 	=	Integer(Message.StringParm)
is_informe	=	Message.StringParm

If is_informe = '5' Then
	st_titulo.text = 'Desverdizado de Lotes'
	dw_1.Object.todos.visible 	= 1
ElseIf is_informe = '7' Then
	st_titulo.text =	'Atemperado de Lotes'
	dw_1.Object.todos.visible 	= 0
End If

If ii_informe = 2 Then
//	rb_1.Visible	=	False
//	rb_2.Visible	=	False
//	rb_3.Visible	=	False
//	rb_4.Visible	=	False
//	dw_1.Object.kilosreales.visible = False
ElseIf ii_informe = 3 Then
//	rb_2.Visible	=	False
//	rb_3.Visible	=	False
//	rb_4.Visible	=	False
	dw_1.Object.todosprot.protect 	= 0
	dw_1.Object.todosprot.visible 	= 1
	dw_1.Object.protocolo.visible 	= 1
	dw_1.Object.t_10.visible 		= 1
End If

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
iuo_Cliente				=	Create uo_Clientesprod

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
If idwc_planta.Retrieve(gi_codexport) = 0 Then
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
End If

//FrigorIfico
dw_1.GetChild("frigorIfico", idwc_frigorIfico)
idwc_frigorIfico.SetTransObject(sqlca)
idwc_frigorIfico.InsertRow(0)

//Camara FrigorIfico
dw_1.GetChild("camara", idwc_camaras)
idwc_camaras.SetTransObject(sqlca)
idwc_camaras.InsertRow(0)

//Especie
dw_1.GetChild("especie", idwc_especie)
idwc_especie.SetTransObject(sqlca)
If idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 Then
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
End If

//Variedad
dw_1.GetChild("variedad",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.InsertRow(0)

//Grupo
dw_1.GetChild("grupo",idwc_grupo)
idwc_grupo.SetTransObject(Sqlca)
If idwc_grupo.Retrieve(0,0) = 0 Then
	MessageBox("Atención","Falta Registrar Grupos")
	idwc_grupo.InsertRow(0)
End If

//Sub Grupo
dw_1.GetChild("subgrupo",idwc_subgrupo)
idwc_subgrupo.SetTransObject(Sqlca)
If idwc_subgrupo.Retrieve(0,0) = 0 Then
	MessageBox("Atención","Falta Registrar Sub Grupos")
	idwc_subgrupo.InsertRow(0)
End If

//Tratamiento
dw_1.GetChild("tratamiento",idwc_tratamiento)
idwc_tratamiento.SetTransObject(Sqlca)
If idwc_tratamiento.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Tratamientos")
	idwc_tratamiento.InsertRow(0)
End If

//Periodo
dw_1.GetChild("periodo",idwc_periodo)
idwc_periodo.SetTransObject(Sqlca)
If idwc_periodo.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Periodos")
	idwc_periodo.InsertRow(0)
End If

//Categorias
dw_1.GetChild("categoria",idwc_categorias)
idwc_categorias.SetTransObject(Sqlca)
If idwc_periodo.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Categorias")
	idwc_categorias.InsertRow(0)
End If
idwc_categorias.Setfilter('isNull( cate_embala ) or  cate_embala <> 1')
idwc_categorias.Filter()

//Productor
dw_1.GetChild("productor",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
If idwc_productor.Retrieve(-1) = 0 Then
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
End If

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

//dw_1.Object.cliente[1] = gi_codexport

dw_1.Object.fechainicio[1]	=	RelativeDate(Today(), -365)
dw_1.Object.fechatermi[1]	=	Today()

If ii_informe = 2 Then dw_1.ModIfy("DataWindow.todosfecha.enabled=0")

end event

type pb_excel from w_para_informes`pb_excel within w_info_desverdizado_lotes
end type

type st_computador from w_para_informes`st_computador within w_info_desverdizado_lotes
end type

type st_usuario from w_para_informes`st_usuario within w_info_desverdizado_lotes
end type

type st_temporada from w_para_informes`st_temporada within w_info_desverdizado_lotes
end type

type p_logo from w_para_informes`p_logo within w_info_desverdizado_lotes
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_desverdizado_lotes
integer width = 1696
string text = "Desverdizado de Lotes"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_desverdizado_lotes
integer x = 2057
integer y = 604
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
 
Integer	li_Planta, li_Especie, li_Grupo, li_SubGrupo, li_protocolo, &
			li_Variedad, li_Periodo, li_Categoria, li_NroLote, &
			li_ConsLote,rb_espe,rb_cam,li_Predio, li_tratesp, &
			li_Cuartel, li_condmerc, li_KilosReales=1, li_condembq
Long		ll_Fila, ll_Productor, ll_Camara, ll_cliente = -1, ll_frigo
String		ls_Tratamiento,ls_titulo
Date		ld_fecini, ld_fecter

istr_info.titulo	= 'EXISTENCIA DE FRUTA GRANEL'

OpenWithParm(vinf, istr_info)

If is_informe ='5' Then
	ls_titulo 					=	"Desverdizado de Lotes"
	vinf.dw_1.DataObject = 	"dw_info_lotes_desverdizado"
ElseIf is_informe = '7' Then
	ls_titulo					=	"Atemperado de Lotes"
	vinf.dw_1.DataObject = 	"dw_info_lotes_atemperado"
End If


If Not dw_1.Object.todos[1] = 1 Then ll_cliente = dw_1.Object.cliente[1]

If IsNull(ll_cliente) Then
	MessageBox( "Cliente Erróneo", "Falta Seleccionar un Cliente.", StopSign!, Ok!)
	Return 1				 
End If

// Acepta Planta //
If dw_1.Object.todosplanta[1] = 1 Then
	li_Planta = -1
Else
	li_Planta = dw_1.Object.planta[1]
	If IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", StopSign!, Ok!)
		Return 1				 
   End If
End If

li_KilosReales	=	dw_1.Object.kilosreales[1]

// Acepta FrigorIfico //
If dw_1.Object.TodosFrigo[1] = 1 Then
	ll_Frigo = -1
	If dw_1.Object.consfrigo[1] = 1 Then
		ll_Frigo = -9
	End If
Else
	ll_Frigo	= dw_1.Object.frigorIfico[1]
	If IsNull(ll_Frigo) Then
		MessageBox( "FrigorIfico Erróneo", "Falta seleccionar un FrigorIfico.", StopSign!, Ok!)
		Return 1				 
   End If
End If	

// Acepta Camara //
If dw_1.Object.todoscama[1] = 1 Then
	ll_Camara = -1
	If dw_1.Object.conscama[1] = 1 Then	
		ll_Camara	=	-9
	End If
Else
	ll_Camara	=	dw_1.Object.camara[1]
	If IsNull(ll_Camara) Then
		MessageBox( "Camara Errónea", "Falta seleccionar una Camara.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Especie //
If dw_1.Object.todosespe[1] = 1 Then
	li_Especie = -1
Else
	li_Especie = dw_1.Object.especie[1]
	If IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Variedad //
If dw_1.Object.todosvari[1] = 1 Then
	li_Variedad = -1
Else
	li_Variedad = dw_1.Object.variedad[1]
	If IsNull(li_Variedad) Then
		MessageBox( "Variedad Errónea", "Falta seleccionar una Variedad.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Grupo de Variedades //
If dw_1.Object.todosvari[1] = 1 Then  
	If dw_1.Object.todosgrupo[1] = 1  Then
		li_Grupo = -1
		If dw_1.Object.consgrupo[1] = 1  Then	
			li_Grupo =	-9
		End If
	Else
		li_Grupo = dw_1.Object.grupo[1]
		If IsNull(li_Grupo) Then
			MessageBox( "Grupo Erróneo", "Falta seleccionar un Grupo de Especie.", StopSign!, Ok!)
			Return 1				 
   		End If
	End If
Else
	li_Grupo = -1
End If

// Acepta SubGrupo de Variedades //
If dw_1.Object.todosvari[1] = 1 Then 
		If dw_1.Object.todossubgru[1] = 1  Then
			li_SubGrupo = -1
			If dw_1.Object.conssubgr[1] = 1 Then
				li_SubGrupo=-1
			End If
		Else
			li_SubGrupo = dw_1.Object.subgrupo[1]
			If IsNull(li_SubGrupo) Then
				MessageBox( "SubGrupo Erróneo", "Falta seleccionar un SubGrupo de Especie.", 	StopSign!, Ok!)
				Return 1				 
   			End If
		End If
	Else
		li_SubGrupo = -1
End If

// Acepta Tratamientos de frio //
If dw_1.Object.todostrata[1] = 1 Then
	ls_Tratamiento = '*'
	If dw_1.Object.constrata[1] = 1 Then	
		ls_Tratamiento = '**'
	End If
Else
	ls_Tratamiento = dw_1.Object.tratamiento[1]
	If IsNull(ls_Tratamiento) or ls_tratamiento="" Then
		MessageBox( "Tratamiento Erróneo", "Falta seleccionar un Tratamiento.", StopSign!, Ok!)
		Return 1				 
   End If
End If
// Acepta Periodo de Frio //
If dw_1.Object.todosperio[1] = 1 Then
	li_Periodo = -1
	If dw_1.Object.consperio[1] = 1 Then	
		li_Periodo = -9
	End If
Else
	li_Periodo = dw_1.Object.periodo[1]
	If IsNull(li_Periodo) Then
		MessageBox( "Período Erróneo", "Falta seleccionar un Período.", StopSign!, Ok!)
		Return 1				 
   End If
End If
// Acepta Categorias
If dw_1.Object.todoscate[1] = 1  Then
	li_Categoria = -1
Else
	li_Categoria = dw_1.Object.categoria[1]
	If IsNull(li_Categoria) Then
		MessageBox( "Categoria Errónea", "Falta seleccionar una Categoria.", StopSign!, Ok!)
		Return 1				 
   End If
End If
// Acepta Productor
If dw_1.Object.todosprod[1] = 1 Then
	ll_Productor = -1
	If dw_1.Object.consprod[1] = 1 Then	
		ll_Productor =	-9
	End If
Else
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", StopSign!, Ok!)
		Return 1				 
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
		MessageBox( "Predio Erróneo", "Falta seleccionar un Predio.", StopSign!, Ok!)
		Return				 
   End If
End If

//Acepta Cuartel
If dw_1.Object.todoscuartel[1] = 1 Then
	li_Cuartel = -1
Else
	li_Cuartel  = dw_1.Object.cuartel[1]
	If IsNull(li_Cuartel ) Then
		MessageBox( "Cuartel Erróneo", "Falta seleccionar un Cuartel.", StopSign!, Ok!)
		Return				 
   End If
End If

//Acepta condicion de mercado
If dw_1.Object.todoscmerc[1] = 1 Then
	li_condmerc = -1
Else
	li_condmerc  = dw_1.Object.merc[1]
	If IsNull(li_condmerc ) Then
		MessageBox( "Mercado Erróneo", "Falta seleccionar una Cond. de Mercado.", StopSign!, Ok!)
		Return				 
   End If
End If

//Acepta condicion de embarque
If dw_1.Object.todosest[1] = 1 Then
	li_condembq = -1
Else
	li_condembq  = dw_1.Object.estado[1]
	If IsNull(li_condembq ) Then
		MessageBox( "Estado Erróneo", "Falta seleccionar un Estado.", StopSign!, Ok!)
		Return				 
   End If
End If

//Acepta desgarchado
If dw_1.Object.tratesptodos[1] = 1 Then
	li_tratesp = -1
Else
	li_tratesp  = dw_1.Object.tratesp[1]
End If

//Acepta desgarchado
If dw_1.Object.todosprot[1] = 1 Then
	li_protocolo = -1
Else
	li_protocolo  = dw_1.Object.protocolo[1]
End If

ld_fecini	=	dw_1.Object.FechaInicio[1]
ld_fecter	=	dw_1.Object.FechaTermi[1]

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Planta,  ll_Frigo, ll_Camara, li_Especie, li_Grupo,	 li_SubGrupo,	li_Variedad, ls_Tratamiento, &
									li_Periodo, li_Categoria,	ll_Productor, li_ConsLote, ll_cliente, li_Predio, 	li_Cuartel, li_condmerc, &
									li_KilosReales,ld_fecini, ld_fecter, li_condembq, li_tratesp, is_informe)
										 
If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)	
	vinf.dw_1.ModIfy("t_titulo.text = '" + ls_titulo + "'")
	vinf.dw_1.Modify('DataWindow.Zoom = 73')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_desverdizado_lotes
integer x = 2048
integer y = 924
integer taborder = 140
end type

type dw_1 from datawindow within w_info_desverdizado_lotes
integer x = 251
integer y = 408
integer width = 1696
integer height = 1164
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_desverdizado_lotes_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged; Integer	li_Null, li_cliente
String	ls_Columna
dw_1.accepttext()
SetNull(li_Null)

li_cliente = dw_1.Object.cliente[1]

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "cliente"
		If Not iuo_Cliente.Existe(Integer(data),True,Sqlca) Then
			This.SetItem(Row, ls_Columna, li_Null )
			Return 1
		End If
		
	Case  "todos" 
		This.Object.cliente[Row] 	= 	li_Null
		
	Case "planta"
		If Not iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) Then
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			Return 1
		Else
			dw_1.Object.todosfrigo.protect = 0
		End If		
		
		//GetChild FrigorIfico
		If idwc_frigorIfico.Retrieve(Integer(data)) = 0 Then
			MessageBox("Atención", "Falta Registrar las camaras")
			idwc_frigorIfico.InsertRow(0)
		Else
			idwc_frigorIfico.SetFilter("cama_codigo = cama_codfri")
			idwc_frigorIfico.Filter()
		End If

		//GetChild Camara
		If idwc_camaras.Retrieve(Integer(data)) = 0 Then
			MessageBox("Atención","Falta Registrar Cámaras Frigorífico Seleccionado")
			idwc_camaras.InsertRow(0)
		End If
		
	Case "frigorIfico"
		If Not iuo_Camarasfrigo.Existe(dw_1.Object.Planta[1], Integer(data), True, Sqlca) Then
			This.SetItem(1, "frigorIfico", li_Null )
			This.SetFocus()
			Return 1
		Else
			dw_1.Object.todoscama.protect = 0
		End If		

		idwc_camaras.SetFilter("cama_codfri = " + data + " And cama_codfri <> cama_codigo") 
		idwc_camaras.Filter()

	Case "camara"
		If NOT iuo_CamarasFrigo.Existe(This.Object.planta[row],Integer(data),True,SqlCa) Then
			This.SetItem(1, "camara", li_Null )
			This.SetFocus()
			Return 1
		Else
			dw_1.Object.todoscama.protect = 0
		End If
						
	Case "especie"
		If NOT iuo_especie.Existe(Integer(data),True,SqlCa) Then
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			Return 1
		Else
			idwc_grupo.Retrieve(Integer(data),0)
			This.Object.Grupo[row]		=	li_Null
			/**/
			idwc_subgrupo.Retrieve(Integer(data),0)
			This.Object.SubGrupo[row]	=	li_Null
			/**/
			idwc_variedad.Retrieve(Integer(data))
			This.Object.Variedad[row]	=	li_Null
		End If

	Case "variedad"
		If NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) Then
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			Return 1
		Else
			This.Object.Grupo[row]			=	iuo_Variedades.Grupo
			This.Object.SubGrupo[row]		=	iuo_Variedades.SubGrupo
		End If
		
	Case "grupo"
		If NOT iuo_GrupoEspecie.Existe(This.Object.especie[row],Integer(data),True,SqlCa) Then
			This.SetItem(1, "grupo", li_Null )
			This.SetFocus()
			Return 1
		Else
			idwc_subgrupo.Retrieve(This.Object.especie[row],Integer(data))
			This.Object.SubGrupo[row]	=	li_Null
		End If
			
	Case "subgrupo"
		If NOT iuo_SubGrupoEspecie.Existe(This.Object.especie[row],&
					This.Object.grupo[row],Integer(data),True,SqlCa) Then
			This.SetItem(1, "subgrupo", li_Null )
			This.SetFocus()
			Return 1
		End If
		
	Case "tratamiento"
		If NOT iuo_TratamientoFrio.Ofp_Recupera_TratamientoFrio(SqlCa,data,True) Then
			This.SetItem(1, "tratamiento", String(li_Null))
			This.SetFocus()
			Return 1
		End If

	Case "periodo"
		If NOT iuo_PeriodoFrio.Ofp_Recupera_PeriodoFrio(SqlCa,Integer(data),True) Then
			This.SetItem(1, "periodo", li_Null)
			This.SetFocus()
			Return 1
		End If
		
	Case "categoria"
		If NOT iuo_Categorias.Existe(Integer(data),True,SqlCa) Then
			This.SetItem(1, "categoria", li_Null)
			This.SetFocus()
			Return 1
		End If

	Case "productor"
		If NOT iuo_Productores.Existe(Long(data),True,SqlCa) Then
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			Return 1
		Else
			This.Object.predio[row]			  =	li_Null
			idwc_predio.Retrieve(Long(Data))
			idwc_condicion.Reset()
		End If
		
	Case "cuartel"
		If NOT iuo_Cuartel.Existe(This.Object.productor[Row],&
									  This.Object.predio[Row],&
									  Integer(data),True,SQLCA) Then
		This.Object.cuartel[Row]	=	li_null
		Return 1
	Else		
		This.Object.Especie[Row]	=	iuo_Cuartel.Especie
		This.Object.variedad[Row]	=	iuo_Cuartel.Variedad
	End If
	
	Case "todosplanta"
		If data='0' Then
			dw_1.Object.todoscama.protect = 1
		ElseIf data = '1' Then 
			This.Object.planta[row]			= li_Null
			dw_1.Object.frigorIfico[row]	= li_Null
			dw_1.Object.todosfrigo[row]	= 1
			dw_1.Object.camara[row]			= li_Null
			dw_1.Object.todoscama[row]		= 1
		End If
		
	Case "todosfrigo"
		If data = '0' Then
			dw_1.Object.todoscama.protect = 1
		ElseIf data = '1' Then
			This.Object.frigorIfico[row]	= li_Null
			dw_1.Object.camara[row]			= li_Null
			dw_1.Object.todoscama[row]		= 1
		End If
 
	Case "todoscama"
		If data='1' Then 
			This.Object.camara[row]	= li_Null
			dw_1.Object.camara[row]	=	li_Null
		End If
	
	Case	"todosespe"
		If	data = '1' Then This.Object.especie[row]	=	li_Null
			dw_1.Object.variedad[row]	= li_Null
			dw_1.Object.todosvari[row]	=	1

	Case	"todostrata"
		If	data = '1' Then This.Object.tratamiento[row]	=	String(li_Null)
	
	Case	"todosperio"
		If	data = '1' Then This.Object.periodo[row]		=	li_Null
	
	Case	"todoscate"
		If	data = '1' Then This.Object.categoria[row]	=	li_Null
			
	Case	"todosprod"
		If	data = '1' Then 
			This.Object.productor[row]		=	li_Null
			This.Object.predio[row]		=	li_Null
			This.Object.cmerc[row]			=	li_Null
			This.Object.Todospredio[row]	=	1
			This.Object.TodosCMerc[row]	=	1
		Else
//			This.Object.Todospredio[row]	=	0
//			This.Object.TodosCMerc[row]	=	0
		End If
	
	Case "predio"
		If NOT iuo_ProdPredio.Existe(Integer(data),This.Object.productor[Row],&
											  True,SQLCA) Then
			This.Object.predio[Row]	=	li_null
			Return 1
		Else		
			idwc_condicion.Retrieve(This.Object.productor[Row],Integer(data),This.Object.especie[row])
			idwc_Cuartel.Retrieve(This.Object.productor[Row],Integer(data))
		End If
	
	Case  "consfrigo" 
		If data = '1' Then 
			This.Object.conscama[row]  = 1
		Else
			This.Object.conscama[row]  = 0
		End If
		
	Case "todosfecha"
		dw_1.Object.fechainicio[1]	=	RelativeDate(Today(), -365)
		dw_1.Object.fechatermi[1]	=	Today()
		
	Case "todosembq"
			This.Object.embq[Row]	=	li_null
		
	Case "fechainicio"
		If Date(data) > dw_1.Object.fechatermi[1] Then
			This.SetItem(1, "fechainicio", RelativeDate(Today(), -365))
			This.SetFocus()
			Return 1
		End If
		
	Case "fechatermino"
		If Date(data) < dw_1.Object.fechainicio[1] Then
			This.SetItem(1, "fechatermino", Today())
			This.SetFocus()
			Return 1
		End If
	
	Case  "tratesptodos" 
		If data = '1' Then 
			This.Object.tratesp.protect 	= 	1
			This.Object.tratesp[row]  	= 	0
		Else
			This.Object.tratesp.protect 	= 	0
			This.Object.tratesp[row]  	= 	0
		End If
	
	Case  "todosprot" 
		This.Object.protocolo[row]  	= 	li_Null
	
	Case  "todosprot" 
		If	data = '1' Then This.Object.estado[row]		=	li_Null
	
	Case	"estado"
		If Integer(Data) > 6 OR Integer(Data) < 0 Then
			MessageBox("Error", "El estado ingresado no es valido", StopSign!)
			If	data = '1' Then This.Object.estado[row]		=	li_Null
		End If
		
	Case  "protocolo" 
		If NOT iuo_protocolo.Existe(Integer(data),True,SQLCa) Then
			This.SetItem(1, "protocolo", li_Null )
			This.SetFocus()
			Return 1
		End If
		
End CHOOSE
end event

event itemerror;RETURN 1
end event

