$PBExportHeader$w_consulta_catastro_superficie.srw
$PBExportComments$consulta de superficie
forward
global type w_consulta_catastro_superficie from w_mant_directo
end type
type dw_2 from datawindow within w_consulta_catastro_superficie
end type
end forward

global type w_consulta_catastro_superficie from w_mant_directo
integer width = 3831
integer height = 2152
string title = "Ficha Consulta Catastro Superficie"
dw_2 dw_2
end type
global w_consulta_catastro_superficie w_consulta_catastro_superficie

type variables

uo_especie          		iuo_especie
uo_variedades       		iuo_variedades
uo_grupoespecie			iuo_grupo
uo_subgrupoespecie		iuo_subgrupo
uo_productores		  		iuo_productores
uo_predios          		iuo_predios
uo_agronomo_productor	iuo_agroespeprod
uo_zonas						iuo_zonas

DataWindowChild idwc_especie, idwc_variedad, idwc_predio, idwc_productor,&
                idwc_pack, idwc_agro, idwc_temp, idwc_grupo, idwc_subgrupo, idwc_zona
					 
				 
Long     il_fila_antes, il_alto_dw1
String	is_nom_esp, is_nom_var, is_nom_pro, is_nom_pre, is_nom_agr, is_nom_pack, is_nom_temp
Double   id_pesone
end variables

forward prototypes
public subroutine buscanombres (integer ssa)
public function boolean buscapacking (integer ai_packing)
public subroutine buscapeso (integer ai_especie, integer ai_variedad)
public function boolean buscatemporada (integer ai_temporada)
public subroutine habilitaenca (boolean ab_habilita)
public function boolean buscaagronomo (integer ai_agronomo)
end prototypes

public subroutine buscanombres (integer ssa);
end subroutine

public function boolean buscapacking (integer ai_packing);
SELECT plde_nombre
  INTO :is_nom_pack
  FROM dba.plantadesp
  WHERE plde_codigo  = :ai_packing
    and plde_tipopl  in (1, 3);

 IF sqlca.sqlcode = -1 THEN
	F_errorBaseDatos(sqlca,"Lectura de Tabla Planatadesp")
	Return False
 ELSEIF sqlca.sqlcode = 100 THEN
	   MessageBox("Atención","No Existe el Código de Packing.Ingrese o Seleccione otro.")
		 RETURN False
 END IF		
	
RETURN TRUE
end function

public subroutine buscapeso (integer ai_especie, integer ai_variedad);Integer li_tipoen, li_envase

//Busca Peso Estándar

 SELECT enva_tipoen, enva_codigo
  into :li_tipoen, :li_envase
  FROM  dba.variedades
 WHERE espe_codigo = :ai_especie    
	AND vari_codigo = :ai_variedad;
 
 IF sqlca.sqlcode = -1 THEN
	F_errorBaseDatos(sqlca,"Lectura de Tabla Variedades")
	
 ELSEIF sqlca.sqlcode <> 100 THEN

	 SELECT enva_pesone
	  into :id_pesone
 	 	FROM  dba.envases
	  WHERE enva_tipoen = :li_tipoen    
		 AND enva_codigo = :li_envase;
		 
	IF sqlca.sqlcode = -1 THEN
		F_errorBaseDatos(sqlca,"Lectura de Tabla Envases")
	ELSE	
		IF Isnull(id_pesone) THEN
		   id_pesone=0
	   END If
   END IF	
 END IF

end subroutine

public function boolean buscatemporada (integer ai_temporada);SELECT pate_nombre
  INTO :is_nom_temp
  FROM dba.paramtemporada
  WHERE pate_tempor  = :ai_temporada;

 IF sqlca.sqlcode = -1 THEN
	F_errorBaseDatos(sqlca,"Lectura de Tabla Temporadas")
	Return False
 ELSEIF sqlca.sqlcode = 100 THEN
	   MessageBox("Atención","No Existe el Código de Temporada.Ingrese o Seleccione otro.")
		 RETURN False
 END IF		
	
RETURN TRUE
end function

public subroutine habilitaenca (boolean ab_habilita);IF ab_habilita THEN
	
//	dw_2.Object.espe_codigo.Protect 				= 0
//	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.vari_codigo.Protect 				= 0
//	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.prod_codigo.Protect 				= 0
//	dw_2.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.prbr_codpre.Protect 				= 0
//	dw_2.Object.prbr_codpre.BackGround.Color	=	RGB(255,255,255)
//	
//	IF gstr_agro.administrador=0 and gstr_agro.codigoagronomo<>0 THEN
//		dw_2.Object.agro_codigo.Protect = 1
//      dw_2.Object.agro_codigo.BackGround.Color	=	RGB(192,192,192)
//	ELSE	
//		dw_2.Object.agro_codigo.Protect 				= 0
//		dw_2.Object.agro_codigo.BackGround.Color	=	RGB(255,255,255)
//	END IF	
	
ELSE

//	dw_2.Object.espe_codigo.Protect 				= 1
//	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.vari_codigo.Protect 				= 1
//	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.prod_codigo.Protect 				= 1
//	dw_2.Object.prod_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.prbr_codpre.Protect 				= 1
//	dw_2.Object.prbr_codpre.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.agro_codigo.Protect 				= 1
//	dw_2.Object.agro_codigo.BackGround.Color	=	RGB(192,192,192)
	
END IF
end subroutine

public function boolean buscaagronomo (integer ai_agronomo);
SELECT agro_nombre
  INTO :is_nom_agr
  FROM dba.agronomos
  WHERE agro_codigo  = :ai_agronomo;

 IF sqlca.sqlcode = -1 THEN
	F_errorBaseDatos(sqlca,"Lectura de Tabla Agronomos")
	Return False
 ELSEIF sqlca.sqlcode = 100 THEN
	   MessageBox("Atención","No Existe el Código de Agronomo.Ingrese o Seleccione otro.")
		 RETURN False
 END IF		
	
RETURN TRUE
end function

on w_consulta_catastro_superficie.create
int iCurrent
call super::create
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
end on

on w_consulta_catastro_superficie.destroy
call super::destroy
destroy(this.dw_2)
end on

event open;/*
istr_mant.argumento[1] = Especie
istr_mant.argumento[2] = Variedad
istr_mant.argumento[3] = Productor
istr_mant.argumento[4] = Predio
istr_mant.argumento[5] = Agronomo
*/

x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 2500
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

istr_mant.Solo_Consulta=True

istr_mant.argumento[1] = ""
istr_mant.argumento[2] = ""
istr_mant.argumento[3] = ""
istr_mant.argumento[4] = ""
istr_mant.argumento[5] = ""

string ls_null
isnull(ls_null)

iuo_especie				= CREATE uo_especie
iuo_variedades			= CREATE uo_variedades
iuo_productores      	= CREATE uo_productores
iuo_predios				= CREATE uo_predios
iuo_agroespeprod 	= CREATE uo_agronomo_productor
iuo_subgrupo			= CREATE uo_subgrupoespecie
iuo_grupo				= CREATE uo_grupoespecie
iuo_zonas				= CREATE uo_zonas

// Especie//
dw_2.GetChild("especie", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(0)=0 THEN
  idwc_especie.insertrow(0)
END IF 	

//Variedad//
dw_2.GetChild("variedad", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.InsertRow(0)

//Grupo//
dw_2.GetChild("grupo", idwc_grupo)
idwc_grupo.SetTransObject(sqlca)
idwc_grupo.InsertRow(0)

//Grupo//
dw_2.GetChild("subgrupo", idwc_subgrupo)
idwc_subgrupo.SetTransObject(sqlca)
idwc_subgrupo.InsertRow(0)

//Productor//
dw_2.GetChild("productor", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(-1)

//Predio//
dw_2.GetChild("predio", idwc_predio)
idwc_predio.SetTransObject(sqlca)
idwc_predio.retrieve(-1)

//agronomo//
dw_2.GetChild("agronomo", idwc_agro)
idwc_agro.SetTransObject(sqlca)
idwc_agro.Retrieve()

//Zona//
dw_2.GetChild("zona", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

dw_2.InsertRow(0)

//IF gstr_agro.codigoagronomo<>0 and isnull(gstr_agro.codigoagronomo)=False THEN
//	dw_2.Object.agro_codigo[1] 	=	gstr_agro.codigoagronomo
//	istr_mant.argumento[5] 			=	string(gstr_agro.codigoagronomo)
//	IF gstr_agro.administrador=0 or isnull(gstr_agro.codigoagronomo) THEN
//		dw_2.Object.agro_codigo.Protect = 1
//	END IF	
//END IF

dw_2.SetFocus()

end event

event ue_recuperadatos;Long	 	ll_fila, respuesta, ll_predio
Integer	li_agro, li_especie, li_variedad, li_grupo, li_subgrupo, li_productor, li_zona, &
    	  		li_grupoprod, li_anoini, li_anofin, &
			li_conduccion, li_riego, li_patron, li_tplanta, li_eurepgap, li_conscuartel, &
			li_edadini1, li_edadfin1, li_edadini2, li_edadfin2, li_edadini3, li_edadfin3, &
			li_edadini4, li_edadfin4, li_edadini5, li_edadfin5, li_consanopta, li_consrangos
DO
   dw_2.Accepttext()
	
	IF	dw_2.Object.todoszona[1]=1 THEN
		li_zona = -1
		IF dw_2.Object.conszona[1] = 1 THEN li_zona = -9
	ELSE
		li_zona = dw_2.Object.zona[1]
		IF isnull(li_zona) or li_zona=0 THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de una Zona.", StopSign!, Ok!)
			Return
		END IF	
	END IF
	
	IF	dw_2.Object.todosprod[1]=1 THEN
		li_productor = -1
		IF dw_2.Object.consprod[1] = 1 THEN li_productor = -9
	ELSE
		li_productor = dw_2.Object.productor[1]
		IF isnull(li_productor) or li_productor=0 THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de un Productor.", StopSign!, Ok!)
			Return 
		END IF	
	END IF
	
	li_grupoprod = dw_2.Object.grupoprod[1] 
		
	IF	dw_2.Object.todospredio[1]=1 THEN
		ll_predio = -1
		IF dw_2.Object.conspredio[1] = 1 THEN ll_predio = -9
	ELSE
		ll_predio = dw_2.Object.predio[1]
		IF isnull(ll_predio) or ll_predio=0 THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de un Predio.", StopSign!, Ok!)
			Return
		END IF	
	END IF
	
	IF dw_2.Object.conscuartel[1] = 1 THEN	li_conscuartel = -9
	
	IF	dw_2.Object.todosagro[1]=1 THEN
		li_agro = -1
		IF dw_2.Object.consagro[1] = 1 THEN li_agro = -9
	ELSE
		li_agro = dw_2.Object.agronomo[1]
		IF isnull(li_agro) or li_agro=0 THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de un Agronómo.", StopSign!, Ok!)
			Return 
		END IF
	END IF 	
	
	IF	dw_2.Object.todoscondu[1]=1 THEN
		li_conduccion = -1
		IF dw_2.Object.conscondu[1] = 1 THEN li_conduccion = -9
	ELSE
		li_conduccion = dw_2.Object.conduccion[1]
		IF isnull(li_conduccion) or li_conduccion=0 THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de un Sistema de Conducción.", StopSign!, Ok!)
			Return 
		END IF
	END IF

	IF	dw_2.Object.todosriego[1]=1 THEN
		li_riego = -1
		IF dw_2.Object.consriego[1] = 1 THEN li_riego = -9
	ELSE
		li_riego = dw_2.Object.riego[1]
		IF isnull(li_riego) THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de un Sistema de Riego.", StopSign!, Ok!)
			Return 
		END IF
	END IF
	
	IF	dw_2.Object.todospatron[1]=1 THEN
		li_patron = -1
		IF dw_2.Object.conspatron[1] = 1 THEN li_patron = -9
	ELSE
		li_patron = dw_2.Object.patron[1]
		IF isnull(li_patron) THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de un Patrón.", StopSign!, Ok!)
			Return 
		END IF
	END IF
	
	IF	dw_2.Object.todostplanta[1]=1 THEN
		li_tplanta = -1
		IF dw_2.Object.constplanta[1] = 1 THEN li_tplanta = -9
	ELSE
		li_tplanta = dw_2.Object.tipoplanta[1]
		IF isnull(li_tplanta) THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de un Tipo Planta.", StopSign!, Ok!)
			Return 
		END IF
	END IF
	
	IF	dw_2.Object.todosespe[1]=1 THEN
		li_especie = -1
	ELSE
		li_especie = dw_2.Object.especie[1]
		IF isnull(li_especie) or li_especie=0 THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de una Especie.", StopSign!, Ok!)
			Return 
		END IF	
	END IF	
	
	IF	dw_2.Object.todosvari[1]=1 THEN
		li_variedad = -1
	ELSE
		li_variedad = dw_2.Object.variedad[1]
		IF isnull(li_variedad) or li_variedad=0 THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de una Variedad.", StopSign!, Ok!)
			Return
		END IF	
	END IF	
	
	IF	dw_2.Object.todosgrupo[1]=1 THEN
		li_grupo = -1
	ELSE
		li_grupo = dw_2.Object.grupo[1]
		IF isnull(li_grupo) or li_grupo=0 THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de un Grupo.", StopSign!, Ok!)
			Return 
		END IF	
	END IF	
	
	IF	dw_2.Object.todossubgru[1]=1 THEN
		li_subgrupo = -1
	ELSE
		li_subgrupo = dw_2.Object.subgrupo[1]
		IF isnull(li_subgrupo) or li_subgrupo=0 THEN
			MessageBox( "Error de Consistencia", "Falta Ingreso de un SubGrupo.", StopSign!, Ok!)
			Return 
		END IF	
	END IF	
	

	li_eurepgap = dw_2.Object.eurepgap[1]
   
	IF dw_2.Object.usarango1[1] = 1 AND dw_2.Object.usarango2[1] = 1 AND &
	   dw_2.Object.usarango3[1] = 1 AND dw_2.Object.usarango4[1] = 1 AND &
		dw_2.Object.usarango5[1] = 1 THEN
		MessageBox( "Error de Consistencia", "A lo menos se debe seleccionar un Rango de Edad.", StopSign!, Ok!)
		Return 
	END IF	
	
	IF dw_2.Object.usarango1[1] = 0 THEN
		li_edadini1 = dw_2.Object.rangoedadini1[1]
		li_edadfin1 = dw_2.Object.rangoedadfin1[1]
	ELSE
		li_edadini1 = -1
		li_edadfin1 = -1
	END IF	
	IF dw_2.Object.usarango2[1] = 0 THEN
		li_edadini2 = dw_2.Object.rangoedadini2[1]
		li_edadfin2 = dw_2.Object.rangoedadfin2[1]
	ELSE
		li_edadini2 = -1
		li_edadfin2 = -1
	END IF	
	IF dw_2.Object.usarango3[1] = 0 THEN
		li_edadini3 = dw_2.Object.rangoedadini31[1]
		li_edadfin3 = dw_2.Object.rangoedadfin3[1]
	ELSE
		li_edadini3 = -1
		li_edadfin3 = -1
	END IF	
	IF dw_2.Object.usarango4[1] = 0 THEN
		li_edadini4 = dw_2.Object.rangoedadini4[1]
		li_edadfin4 = dw_2.Object.rangoedadfin4[1]
	ELSE
		li_edadini4 = -1
		li_edadfin4 = -1
	END IF	
	IF dw_2.Object.usarango5[1] = 0 THEN
		li_edadini5 = dw_2.Object.rangoedadini5[1]
		li_edadfin5 = dw_2.Object.rangoedadfin5[1]
	ELSE
		li_edadini5 = -1
		li_edadfin5 = -1
	END IF	
	
	li_anoini = dw_2.Object.anoini[1]
	li_anofin = dw_2.Object.anofin[1]
   
	IF dw_2.Object.consano[1] = 1 THEN	li_consanopta = -9
	IF dw_2.Object.consrango[1] = 1 THEN	li_consrangos = -9
	
   ll_fila  = dw_1.Retrieve(li_zona, li_productor, li_grupoprod, ll_predio, li_conscuartel, &
	                         li_agro, li_conduccion, li_riego, li_patron, li_tplanta, &
									 li_especie, li_variedad, li_grupo, li_subgrupo, li_eurepgap, &
									 li_edadini1, li_edadfin1, li_edadini2, li_edadfin2, &
									 li_edadini3, li_edadfin3, li_edadini4, li_edadfin4, &
									 li_edadini5, li_edadfin5, li_anoini, li_anofin, li_consanopta, &
									 li_consrangos)
					 
  	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_grabar.Enabled		= True
		pb_imprimir.Enabled	= True
		pb_eliminar.enabled  = TRUE
		
		IF li_productor = -9 AND ll_predio = -9 AND li_zona = -9 AND li_Agro = -9 THEN
			dw_1.Modify("DataWindow.Header.2.Height=0")
			dw_1.Modify("DataWindow.Trailer.2.Height=0")
		END IF
		
		IF li_consrangos = -9 THEN
			dw_1.Modify("DataWindow.Header.1.Height=0")
		END IF
		
//		Habilitaenca(FALSE)
	ELSEIF	ll_fila = 0 THEN
		MessageBox("Atención","No existe Información con parámetros seleccionados.")
	END IF
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila, ll_fila, ll_predio
Integer li_agro, li_especie, li_variedad, li_grupo, li_subgrupo, li_productor, li_zona, &
    	  li_grupoprod, li_anoini, li_anofin, &
		li_conduccion, li_riego, li_patron, li_tplanta, li_eurepgap, li_conscuartel, &
		li_edadini1, li_edadfin1, li_edadini2, li_edadfin2, li_edadini3, li_edadfin3, &
		li_edadini4, li_edadfin4, li_edadini5, li_edadfin5, li_consanopta, li_consrangos

str_info	lstr_info

lstr_info.titulo	= "CONSULTA CATASTRO SUPERFICIE"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_pronostico_cosecha_consulta"

vinf.dw_1.SetTransObject(sqlca)

dw_2.Accepttext()

IF	dw_2.Object.todoszona[1]=1 THEN
	li_zona = -1
	IF dw_2.Object.conszona[1] = 1 THEN li_zona = -9
ELSE
	li_zona = dw_2.Object.zona[1]
	IF isnull(li_zona) or li_zona=0 THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de una Zona.", StopSign!, Ok!)
		Return
	END IF	
END IF

IF	dw_2.Object.todosprod[1]=1 THEN
	li_productor = -1
	IF dw_2.Object.consprod[1] = 1 THEN li_productor = -9
ELSE
	li_productor = dw_2.Object.productor[1]
	IF isnull(li_productor) or li_productor=0 THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de un Productor.", StopSign!, Ok!)
		Return 
	END IF	
END IF

li_grupoprod = dw_2.Object.grupoprod[1] 
	
IF	dw_2.Object.todospredio[1]=1 THEN
	ll_predio = -1
	IF dw_2.Object.conspredio[1] = 1 THEN ll_predio = -9
ELSE
	ll_predio = dw_2.Object.predio[1]
	IF isnull(ll_predio) or ll_predio=0 THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de un Predio.", StopSign!, Ok!)
		Return
	END IF	
END IF

IF dw_2.Object.conscuartel[1] = 1 THEN	li_conscuartel = -9

IF	dw_2.Object.todosagro[1]=1 THEN
	li_agro = -1
	IF dw_2.Object.consagro[1] = 1 THEN li_agro = -9
ELSE
	li_agro = dw_2.Object.agronomo[1]
	IF isnull(li_agro) or li_agro=0 THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de un Agronómo.", StopSign!, Ok!)
		Return 
	END IF
END IF 	

IF	dw_2.Object.todoscondu[1]=1 THEN
	li_conduccion = -1
	IF dw_2.Object.conscondu[1] = 1 THEN li_conduccion = -9
ELSE
	li_conduccion = dw_2.Object.conduccion[1]
	IF isnull(li_conduccion) or li_conduccion=0 THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de un Sistema de Conducción.", StopSign!, Ok!)
		Return 
	END IF
END IF

IF	dw_2.Object.todosriego[1]=1 THEN
	li_riego = -1
	IF dw_2.Object.consriego[1] = 1 THEN li_riego = -9
ELSE
	li_riego = dw_2.Object.riego[1]
	IF isnull(li_riego) THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de un Sistema de Riego.", StopSign!, Ok!)
		Return 
	END IF
END IF

IF	dw_2.Object.todospatron[1]=1 THEN
	li_patron = -1
	IF dw_2.Object.conspatron[1] = 1 THEN li_patron = -9
ELSE
	li_patron = dw_2.Object.patron[1]
	IF isnull(li_patron) THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de un Patrón.", StopSign!, Ok!)
		Return 
	END IF
END IF

IF	dw_2.Object.todostplanta[1]=1 THEN
	li_tplanta = -1
	IF dw_2.Object.constplanta[1] = 1 THEN li_tplanta = -9
ELSE
	li_tplanta = dw_2.Object.tipoplanta[1]
	IF isnull(li_tplanta) THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de un Tipo Planta.", StopSign!, Ok!)
		Return 
	END IF
END IF

IF	dw_2.Object.todosespe[1]=1 THEN
	li_especie = -1
ELSE
	li_especie = dw_2.Object.especie[1]
	IF isnull(li_especie) or li_especie=0 THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de una Especie.", StopSign!, Ok!)
		Return 
	END IF	
END IF	

IF	dw_2.Object.todosvari[1]=1 THEN
	li_variedad = -1
ELSE
	li_variedad = dw_2.Object.variedad[1]
	IF isnull(li_variedad) or li_variedad=0 THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de una Variedad.", StopSign!, Ok!)
		Return
	END IF	
END IF	

IF	dw_2.Object.todosgrupo[1]=1 THEN
	li_grupo = -1
ELSE
	li_grupo = dw_2.Object.grupo[1]
	IF isnull(li_grupo) or li_grupo=0 THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de un Grupo.", StopSign!, Ok!)
		Return 
	END IF	
END IF	

IF	dw_2.Object.todossubgru[1]=1 THEN
	li_subgrupo = -1
ELSE
	li_subgrupo = dw_2.Object.subgrupo[1]
	IF isnull(li_subgrupo) or li_subgrupo=0 THEN
		MessageBox( "Error de Consistencia", "Falta Ingreso de un SubGrupo.", StopSign!, Ok!)
		Return 
	END IF	
END IF	


li_eurepgap = dw_2.Object.eurepgap[1]


IF dw_2.Object.usarango1[1] = 0 THEN
	li_edadini1 = dw_2.Object.rangoedadini1[1]
	li_edadfin1 = dw_2.Object.rangoedadfin1[1]
ELSE
	li_edadini1 = -1
	li_edadfin1 = -1
END IF	
IF dw_2.Object.usarango2[1] = 0 THEN
	li_edadini2 = dw_2.Object.rangoedadini2[1]
	li_edadfin2 = dw_2.Object.rangoedadfin2[1]
ELSE
	li_edadini2 = -1
	li_edadfin2 = -1
END IF	
IF dw_2.Object.usarango3[1] = 0 THEN
	li_edadini3 = dw_2.Object.rangoedadini31[1]
	li_edadfin3 = dw_2.Object.rangoedadfin3[1]
ELSE
	li_edadini3 = -1
	li_edadfin3 = -1
END IF	
IF dw_2.Object.usarango4[1] = 0 THEN
	li_edadini4 = dw_2.Object.rangoedadini4[1]
	li_edadfin4 = dw_2.Object.rangoedadfin4[1]
ELSE
	li_edadini4 = -1
	li_edadfin4 = -1
END IF	
IF dw_2.Object.usarango5[1] = 0 THEN
	li_edadini5 = dw_2.Object.rangoedadini5[1]
	li_edadfin5 = dw_2.Object.rangoedadfin5[1]
ELSE
	li_edadini5 = -1
	li_edadfin5 = -1
END IF	


li_anoini = dw_2.Object.anoini[1]
li_anofin = dw_2.Object.anofin[1]

IF dw_2.Object.consano[1] = 1 THEN	li_consanopta = -9
IF dw_2.Object.consrango[1] = 1 THEN	li_consrangos = -9
	
fila = vinf.dw_1.Retrieve(li_zona, li_productor, li_grupoprod, ll_predio, li_conscuartel, &
	                         li_agro, li_conduccion, li_riego, li_patron, li_tplanta, &
									 li_especie, li_variedad, li_grupo, li_subgrupo, li_eurepgap, &
									 li_edadini1, li_edadfin1, li_edadini2, li_edadfin2, &
									 li_edadini3, li_edadfin3, li_edadini4, li_edadfin4, &
									 li_edadini5, li_edadfin5, li_anoini, li_anofin, li_consanopta, &
									 li_consrangos)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	
	F_Membrete(vinf.dw_1)
	IF li_productor = -9 AND ll_predio = -9 AND li_zona = -9 AND li_Agro = -9 THEN
//		vinf.dw_1.Modify("DataWindow.Header.2.Height=0")
		vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
	END IF
		
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event closequery;//
end event

event resize;Integer		li_posic_x, li_posic_y, &
				li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

dw_1.Resize(This.WorkSpaceWidth() - 490,This.WorkSpaceHeight() - dw_1.y - 75)

dw_1.x					=	78
st_encabe.x				=	dw_1.x
st_encabe.width		=	dw_1.width

li_posic_x				=	This.WorkSpaceWidth() - 370
IF st_encabe.Visible THEN
	li_posic_y				=	st_encabe.y
ELSE
	li_posic_y				=	dw_1.y
END IF

pb_lectura.x				=	li_posic_x
pb_lectura.y				=	li_posic_y
pb_lectura.width		=	li_Ancho
pb_lectura.height		=	li_Alto

li_posic_y 				+= li_Siguiente * 1.25

IF pb_nuevo.Visible THEN
	pb_nuevo.x			=	li_posic_x
	pb_nuevo.y			=	li_posic_y
	pb_nuevo.width	=	li_Ancho
	pb_nuevo.height	=	li_Alto
	li_posic_y 			+= li_Siguiente
END IF

IF pb_insertar.Visible THEN
	pb_insertar.x		=	li_posic_x
	pb_insertar.y		=	li_posic_y
	pb_insertar.width	=	li_Ancho
	pb_insertar.height	=	li_Alto
	li_posic_y += li_Siguiente
END IF

IF pb_eliminar.Visible THEN
	pb_eliminar.x			=	li_posic_x
	pb_eliminar.y			=	li_posic_y
	pb_eliminar.width		=	li_Ancho
	pb_eliminar.height	=	li_Alto
	li_posic_y += li_Siguiente
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				=	li_posic_x
	pb_grabar.y				=	li_posic_y
	pb_grabar.width		=	li_Ancho
	pb_grabar.height		=	li_Alto
	li_posic_y += li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_posic_y += li_Siguiente
END IF

pb_salir.x				=	li_posic_x
pb_salir.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_salir.width			=	li_Ancho
pb_salir.height			=	li_Alto
end event

type st_encabe from w_mant_directo`st_encabe within w_consulta_catastro_superficie
boolean visible = false
integer x = 55
integer y = 24
integer width = 3077
integer height = 324
integer taborder = 10
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_consulta_catastro_superficie
integer x = 3241
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;
pb_grabar.enabled		=	False
pb_imprimir.enabled	=	False
pb_eliminar.enabled  =  FALSE


Habilitaenca(TRUE)
end event

type pb_lectura from w_mant_directo`pb_lectura within w_consulta_catastro_superficie
integer x = 3241
integer taborder = 20
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_consulta_catastro_superficie
string tag = "Exportar Excel"
integer x = 3241
integer y = 788
integer taborder = 0
string picturename = "\Desarrollo 12\Imagenes\Botones\excel.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\excel.png"
end type

event pb_eliminar::clicked;IF dw_1.RowCount() > 0 THEN
	dw_1.SaveAs("",Excel5!,TRUE)
END IF	
end event

type pb_insertar from w_mant_directo`pb_insertar within w_consulta_catastro_superficie
boolean visible = false
integer x = 3250
integer y = 1332
integer taborder = 0
end type

type pb_salir from w_mant_directo`pb_salir within w_consulta_catastro_superficie
integer x = 3241
integer y = 1792
integer taborder = 100
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_consulta_catastro_superficie
integer x = 3241
integer y = 600
integer taborder = 90
end type

type pb_grabar from w_mant_directo`pb_grabar within w_consulta_catastro_superficie
boolean visible = false
integer x = 3250
integer taborder = 80
end type

type dw_1 from w_mant_directo`dw_1 within w_consulta_catastro_superficie
integer x = 46
integer y = 968
integer width = 3086
integer height = 1016
string dataobject = "dw_mues_pronostico_cosecha_consulta"
boolean hscrollbar = true
end type

type dw_2 from datawindow within w_consulta_catastro_superficie
integer x = 55
integer y = 24
integer width = 2999
integer height = 904
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_cons_superficie_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String  	ls_columna
Integer	li_prov, li_region, li_comu, ll_fila, li_null
Long 		respuesta

SetNull(li_null)

ls_columna = dwo.name 

Choose Case ls_columna		
	Case 	"agronomo"		
		If BuscaAgronomo(Integer(data))=False Then
			dw_2.Setitem(1,"agronomo",li_null)
			Return 1
		End If
	
	Case "especie"
		If Not iuo_especie.existe(Integer(data),True,sqlca) Then
			dw_2.setitem(1,"variedad", li_Null)
			dw_2.Setitem(1,"especie",li_null)
			Return 1
		Else
			dw_2.GetChild("variedad",idwc_variedad)
			idwc_variedad.SetTransObject(Sqlca)
			If idwc_variedad.Retrieve(integer(data)) = 0 Then
				MessageBox("Atención","Falta Registrar Variedades asociadas a la Especie.")
				idwc_variedad.InsertRow(0)
			End If
			dw_2.GetChild("grupo",idwc_grupo)
			idwc_grupo.SetTransObject(Sqlca)
			If idwc_grupo.Retrieve(integer(data),0) = 0 Then
				idwc_grupo.InsertRow(0)
			End If
		End If 
	
	Case "grupo"
		If NOT iuo_Grupo.Existe(This.Object.especie[row],Integer(data),True,SqlCa) Then
			This.SetItem(1, "grupo", li_Null )
			This.SetFocus()
			Return 1
		Else
			dw_2.GetChild("subgrupo",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			If idwc_subgrupo.Retrieve(dw_2.Object.especie[1],integer(data)) = 0 Then
				idwc_subgrupo.InsertRow(0)
			End If
			This.Object.subgrupo[row]	=	li_Null
		End If
			
	Case "subgrupo"
		If NOT iuo_SubGrupo.Existe(This.Object.especie[row],&
					                  This.Object.grupo[row],Integer(data),True,SqlCa) Then
			This.SetItem(1, "subgrupo", li_Null )
			This.SetFocus()
			Return 1
		End If
	
	Case "zona"
		If NOT iuo_zonas.Existe(Integer(data),True,SqlCa) Then
			This.SetItem(1, "zona", li_Null )
			This.SetFocus()
			Return 1
		End If
		
	Case "productor"
		If Not iuo_productores.Existe(Integer(Data),True,sqlca) Then
			dw_2.setitem(1,"predio", li_Null)
			dw_2.SetItem(1,"productor", li_null)
			Return 1
		Else
			dw_2.GetChild("predio", idwc_predio)
			idwc_predio.SetTransObject(sqlca)
			idwc_predio.retrieve(iuo_Productores.Codigo)
		End If
		 
	Case "variedad"		
		If Not iuo_variedades.existe(this.Object.especie[1],Integer(data),True,Sqlca) Then
			this.SetItem(1,"variedad", li_null)
			Return 1
		Else
			dw_2.GetChild("subgrupo",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			If idwc_subgrupo.Retrieve(dw_2.Object.especie[1],iuo_variedades.subgrupo) = 0 Then
				idwc_subgrupo.InsertRow(0)
			End If
			dw_2.Object.grupo[1] = iuo_variedades.grupo
			dw_2.Object.subgrupo[1] = iuo_variedades.subgrupo
			dw_2.Object.todosgrupo[1] = 0
			dw_2.Object.todossubgru[1] = 0			
		End If 
			
	Case "predio"
		If Not iuo_predios.existe(sqlca,True,Long(data)) Then
			This.SetItem(1,"predio", li_null)
			Return 1
		End If 
	
	Case "todoszona"
		If data = "1" Then This.Object.zona[row] = li_null
	
	Case "todosprod"
		If data = "1" Then
			This.Object.productor[row] 	= li_null
			This.Object.predio[row]			= li_null
			This.Object.todospredio[row]	= 1
		End If	
		
	Case "todospredio"
		If data = "1" Then This.Object.predio[row]	= li_Null
		
	Case "todosagro"
		If data = "1" Then This.Object.agronomo[row] = li_Null
		
	Case "todoscondu"
		If data = "1" Then This.Object.conduccion[row] = li_Null
		
	Case "todosriego" 
		If data = "1" Then This.Object.riego[row]	= li_Null
	
	Case "todospatron"
		If data = "1" Then This.Object.patron[row] = li_Null
		
	Case "todostplanta"
		If data = "1" Then This.Object.tipoplanta[row] = li_Null
	
	Case "todosespe"
		If data = "1" Then
			This.Object.especie[row]	= li_Null
			This.Object.variedad[row]	= li_Null
			This.Object.grupo[row]		= li_Null
			This.Object.subgrupo[row]	= li_Null
			This.Object.todossubgru[row]= 1
			This.Object.todosgrupo[row] = 1
			This.Object.todosvari[row]	= 1
		End If
	
	Case "todosvari"
		If data = "1" Then
			This.Object.variedad[row]	= li_Null
			This.Object.grupo[row]		= li_Null
			This.Object.subgrupo[row]	= li_Null
			This.Object.todossubgru[row]= 1
			This.Object.todosgrupo[row] = 1
		End If 	

	Case "todosgrupo"
		If data = "1" Then
			This.Object.grupo[row]		= li_Null
			This.Object.subgrupo[row]	= li_Null
			This.Object.todossubgru[row]= 1
		End If
		
	Case "todossubgru"
		If data = "1" Then This.Object.subgrupo[row]	= li_Null
	
	Case "todoscondu"
		If data = "1" Then This.Object.conduccion[row]	= li_Null
	
	Case "todosriego"
		If data = "1" Then This.Object.riego[row]	= li_Null
	
	Case "todospatron"
		If data = "1" Then This.Object.patron[row]	= li_Null
	
	Case "todostplanta"
		If data = "1" Then This.Object.tipoplanta[row]	= li_Null
		
End CHOOSE
end event

event itemerror;Return 1
end event

